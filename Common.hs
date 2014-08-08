{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Common (
	toJid,
	fromJid,
	fromCommon,
	toCommon,
	Common(..), Tag(..), Mechanism(..),
	XmppCommon(..),
	Requirement(..),
	Feature(..), Bind(..), Jid(..), Query(..),
	Roster(..), Identity(..), IdentityTag(..),
	DiscoTag(..), InfoFeature(..), InfoFeatureTag(..),
	IqType(..),
	MessageBody(..),
	MessageDelay(..), DelayTag(..), toDelay,
	MessageXDelay(..), XDelayTag(..), toXDelay,
	MBody(..),
	MessageType(..),
	nullQ,
	) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import Digest
import Papillon

import XmppCommon

fromJust' :: String -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' em _ = error em

toDiscoTag :: QName -> DiscoTag
toDiscoTag ((_, Just "http://jabber.org/protocol/disco#info"), "node") = DTNode
toDiscoTag n = DTRaw n

toInfoFeature :: XmlNode -> Maybe InfoFeature
toInfoFeature (XmlNode ((_, Just "http://jabber.org/protocol/disco#info"),
	"feature") _ as []) = Just $ case map (first toInfoFeatureTag) as of
		[(IFTVar, v)] -> InfoFeature v
		atts -> InfoFeatureSemiRaw atts
toInfoFeature _n = Nothing -- InfoFeatureRaw n

toInfoFeatureTag :: QName -> InfoFeatureTag
toInfoFeatureTag ((_, Just "http://jabber.org/protocol/disco#info"), "var") = IFTVar
toInfoFeatureTag n = IFTVarRaw n

toIdentityTag :: QName -> IdentityTag
toIdentityTag ((_, Just "http://jabber.org/protocol/disco#info"), "type") = IDTType
toIdentityTag ((_, Just "http://jabber.org/protocol/disco#info"), "name") = IDTName
toIdentityTag ((_, Just "http://jabber.org/protocol/disco#info"), "category") =
	IDTCategory
toIdentityTag n = IDTRaw n

toIdentity :: XmlNode -> Maybe Identity
toIdentity (XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "identity")
	_ as []) = Just . Identity $ map (first toIdentityTag) as
toIdentity _n = Nothing -- IdentityRaw n

toIqBody :: [XmlNode] -> Query
toIqBody [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ []
	[n]] = IqBind Nothing $ toBind n
toIqBody [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ []
	[n, n']] | r <- toRequirement [n] = IqBind (Just r) $ toBind n'
toIqBody [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-session"), "session")
	_ [] []] = IqSession
toIqBody [XmlNode ((_, Just "jabber:iq:roster"), "query") _ [] []] =
	IqRoster Nothing
toIqBody [XmlNode ((_, Just "jabber:iq:roster"), "query") _ as ns] = IqRoster
	. Just $ Roster (snd <$> find (\((_, v), _) -> v == "ver") as) ns
toIqBody [XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "query")
	_ [] []] = IqDiscoInfo
toIqBody [XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "query")
	_ as []] = IqDiscoInfoNode $ map (first toDiscoTag) as
toIqBody [XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "query")
	_ as ns] = IqDiscoInfoFull
	(map (first toDiscoTag) as)
	(catMaybes $ map toIdentity ns)
	(catMaybes $ map toInfoFeature ns)
	(filter (\n -> isNothing (toIdentity n) && isNothing (toInfoFeature n)) ns)
toIqBody [] = IqSessionNull
toIqBody ns = QueryRaw ns

toBind :: XmlNode -> Bind
toBind (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "resource") [] []
	[XmlCharData cd]) = Resource cd
toBind n = BindRaw n

fromBind :: Bind -> [XmlNode]
fromBind (BJid _) = error "fromBind: not implemented"
fromBind (Resource r) = [
	XmlNode (nullQ "bind") [("", "urn:ietf:params:xml:ns:xmpp-bind")] []
		[XmlNode (nullQ "required") [] [] [], resource r]
	]
fromBind (BindRaw n) = [n]

resource :: BS.ByteString -> XmlNode
resource r = XmlNode (nullQ "resource") [] [] [XmlCharData r]

drToXmlNode :: DigestResponse -> XmlNode
drToXmlNode dr = XmlNode (("", Nothing), "response")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] []
	[XmlCharData . encode . kvsToS $ responseToKvs True dr]

drnToXmlNode :: XmlNode
drnToXmlNode = XmlNode (nullQ "response")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []

fromQuery :: Query -> [XmlNode]
fromQuery (IqBind Nothing (BJid j)) =
	[XmlNode (nullQ "jid") [] [] [XmlCharData $ fromJid j]]
fromQuery (IqRoster (Just (Roster mv ns))) = (: []) $
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] as ns
	where as = case mv of
		Just v -> [(nullQ "ver", v)]
		_ -> []
fromQuery (IqBind r b) = maybe id ((:) . fromRequirement) r $ fromBind b
fromQuery IqSession = [session]
fromQuery (IqRoster Nothing) = [roster]
fromQuery (IqCapsQuery v n) = [capsQuery v n]
fromQuery (IqCapsQuery2 ns) = ns
fromQuery IqSessionNull = []
fromQuery (QueryRaw ns) = ns
fromQuery _ = error "fromQuery: not implemented yet"

fromIqType :: IqType -> BS.ByteString
fromIqType Get = "get"
fromIqType Set = "set"
fromIqType Result = "result"
fromIqType ITError = "error"

toIqType :: BS.ByteString -> IqType
toIqType "get" = Get
toIqType "set" = Set
toIqType "result" = Result
toIqType "error" = ITError
toIqType t = error $ "toIqType: unknown iq type " ++ show t

iqTypeToAtt :: IqType -> (QName, BS.ByteString)
iqTypeToAtt = (nullQ "type" ,) . fromIqType

fromChallenge :: BS.ByteString -> BS.ByteString ->
	BS.ByteString -> BS.ByteString -> BS.ByteString -> [XmlNode]
fromChallenge r u q c a = (: []) . XmlCharData . B64.encode $ BS.concat [
	"realm=", BSC.pack $ show r, ",",
	"nonce=", BSC.pack $ show u, ",",
	"qop=", BSC.pack $ show q, ",",
	"charset=", c, ",", "algorithm=", a ] -- md5-sess" ]

toCommon :: XmlNode -> Common
toCommon (XmlDecl (1, 0)) = CCommon XCDecl
toCommon (XmlStart ((_, Just "http://etherx.jabber.org/streams"), "stream") _ as) =
	CCommon . XCBegin $ map (first toTag) as
toCommon (XmlEnd ((_, Just "http://etherx.jabber.org/streams"), "stream")) =
	CCommon XCEnd
toCommon (XmlNode ((_, Just "http://etherx.jabber.org/streams"), "features")
	_ [] nds) = CCommon . XCFeatures $ map toFeature nds
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls")
	_ [] []) = CCommon XCStarttls
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed")
	_ [] []) = CCommon XCProceed

toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth")
	_ as [])
	| [(Mechanism, m)] <- map (first toTag) as =
		CCommon . XCAuth $ toMechanism' m

toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "challenge")
	_ [] []) = SRChallengeNull
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "challenge")
	_ [] [XmlCharData c]) = let
		Right d = B64.decode c
		Just a = parseAtts d in
		case a of
			[("rspauth", ra)] -> SRChallengeRspauth ra
--			_ -> error $ "hoge: " ++ show a
			_ -> SRChallenge {
				realm = fromJust' "3" $ lookup "realm" a,
				nonce = fromJust' "4" $ lookup "nonce" a,
				qop = fromJust' "5" $ lookup "qop" a,
				charset = fromJust' "6" $ lookup "charset" a,
				algorithm = fromJust' "7" $ lookup "algorithm" a }
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "response")
	_ [] [XmlCharData cd]) = let
		Just a = parseAtts . (\(Right s) -> s) $ B64.decode cd
		in
		SRResponse (fromJust $ lookup "response" a) DR {
			drUserName = fromJust $ lookup "username" a,
			drRealm = fromJust $ lookup "realm" a,
			drPassword = "password",
			drCnonce = fromJust $ lookup "cnonce" a,
			drNonce = fromJust $ lookup "nonce" a,
			drNc = fromJust $ lookup "nc" a,
			drQop = fromJust $ lookup "qop" a,
			drDigestUri = fromJust $ lookup "digest-uri" a,
			drCharset = fromJust $ lookup "charset" a }
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "response")
	_ [] []) = SRResponseNull
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "success")
	_ [] []) = CCommon XCSaslSuccess
toCommon (XmlNode ((_, Just "jabber:client"), "iq") _ as ns) =
	SRIq tp i fr to $ toIqBody ns
	where
	ts = map (first toTag) as
	tp = toIqType . fromJust $ lookup Type ts
	Just i = lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid <$> lookup To ts
toCommon (XmlNode ((_, Just "jabber:client"), "presence") _ as ns) =
	SRPresence (map (first toTag) as) ns

toCommon (XmlNode ((_, Just "jabber:client"), "message") _ as [b, d, xd])
	| XmlNode ((_, Just "jabber:client"), "body") _ [] _ <- b,
		XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ _ [] <- d,
		XmlNode ((_, Just "jabber:x:delay"), "x") _ _ [] <- xd =
		CCommon . XCMessage tp i fr to $
			MBodyDelay (toBody b) (toDelay d) (toXDelay xd)
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
toCommon (XmlNode ((_, Just "jabber:client"), "message") _ as ns) =
	CCommon . XCMessage tp i fr to $ MBodyRaw ns
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
toCommon n = CCommon $ XCRaw n

capsQuery :: BS.ByteString -> BS.ByteString -> XmlNode
capsQuery v n = XmlNode (("", Nothing), "query")
	[("", "http://jabber.org/protocol/disco#info")]
	[((("", Nothing), "node"), n `BS.append` "#" `BS.append` v)] []

roster :: XmlNode
roster = XmlNode (nullQ "query") [("", "jabber:iq:roster")] [] []

session :: XmlNode
session = XmlNode (nullQ "session")
	[("", "urn:ietf:params:xml:ns:xmpp-session")] [] []

fromCommon :: Common -> XmlNode
fromCommon (CCommon XCDecl) = XmlDecl (1, 0)
fromCommon (CCommon (XCBegin as)) = XmlStart (("stream", Nothing), "stream")
	[	("", "jabber:client"),
		("stream", "http://etherx.jabber.org/streams") ]
	(map (first fromTag) as)
fromCommon (CCommon (XCFeatures fs)) = XmlNode
	(("stream", Nothing), "features") [] [] $ map fromFeature fs
fromCommon (CCommon (XCAuth m)) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[((("", Nothing), "mechanism"), fromMechanism' m)] []
fromCommon SRChallengeNull = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon c@SRChallenge{} = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] $ fromChallenge
		(realm c) (nonce c) (qop c) (charset c) (algorithm c)
fromCommon (SRResponse _ dr) = drToXmlNode dr
fromCommon (SRChallengeRspauth sret) = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] [XmlCharData sret]
fromCommon SRResponseNull = drnToXmlNode
fromCommon (CCommon XCSaslSuccess) =
	XmlNode (nullQ "success") [("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon (SRIq tp i fr to q) = XmlNode (nullQ "iq") []
	(catMaybes [
		Just $ iqTypeToAtt tp,
		Just (nullQ "id", i),
		(nullQ "from" ,) . fromJid <$> fr,
		(nullQ "to" ,) . fromJid <$> to ])
	(fromQuery q)
fromCommon (SRPresence ts c) =
	XmlNode (nullQ "presence") [] (map (first fromTag) ts) c
fromCommon (CCommon (XCMessage tp i fr to (MBody (MessageBody m)))) =
	XmlNode (nullQ "message") []
		(catMaybes [
			Just $ messageTypeToAtt tp,
			Just (nullQ "id", i),
			(nullQ "from" ,) . fromJid <$> fr,
			Just (nullQ "to", fromJid to) ])
		[XmlNode (nullQ "body") [] [] [XmlCharData m]]
fromCommon (CCommon (XCMessage Chat i fr to (MBodyRaw ns))) =
	XmlNode (nullQ "message") []
		(catMaybes [
			Just (nullQ "type", "chat"),
			Just (nullQ "id", i),
			(nullQ "from" ,) . fromJid <$> fr,
			Just (nullQ "to", fromJid to) ]) ns
fromCommon (CCommon XCEnd) = XmlEnd (("stream", Nothing), "stream")
fromCommon (CCommon (XCRaw n)) = n
fromCommon c = error $ "fromCommon: not implemented yet: " ++ show c
