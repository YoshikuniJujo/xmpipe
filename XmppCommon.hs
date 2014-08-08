{-# LANGUAGE OverloadedStrings, TupleSections #-}

module XmppCommon (
	toCommon, fromCommon,
	Side(..), jabberQ,
	Tag(..), toTag, fromTag,
	Requirement(..), toRequirement, fromRequirement,
	Feature(..), toFeature, fromFeature,
	Mechanism(..), toMechanism, fromMechanism,
		toMechanism', fromMechanism',
	MessageType(..), toMessageType, fromMessageType, messageTypeToAtt,
	Jid(..), toJid, fromJid,
	MessageBody(..), toBody,
	MBody(..),
	MessageDelay(..), DelayTag(..), toDelay,
	MessageXDelay(..), XDelayTag(..), toXDelay,
	nullQ,

	Common(..),

	Query(..),
	IqType(..),
	Roster(..),
	Bind(..),
	Identity(..),
	IdentityTag(..),
	InfoFeature(..),
	InfoFeatureTag(..),
	DiscoTag(..),

	fromBind, iqTypeToAtt,
	) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import Digest
import Papillon

data Common
	= XCDecl
	| XCBegin [(Tag, BS.ByteString)]
	| XCEnd
	| XCFeatures [Feature]
	| XCStarttls
	| XCProceed
	| XCAuth Mechanism
	| XCSaslSuccess
	| XCMessage MessageType BS.ByteString (Maybe Jid) Jid MBody
	| XCRaw XmlNode

	| SRChallengeNull
	| SRChallenge {
		realm :: BS.ByteString,
		nonce :: BS.ByteString,
		qop :: BS.ByteString,
		charset :: BS.ByteString,
		algorithm :: BS.ByteString }
	| SRResponse BS.ByteString DigestResponse
	| SRChallengeRspauth BS.ByteString
	| SRResponseNull
	| SRIq IqType BS.ByteString (Maybe Jid) (Maybe Jid) Query
	| SRPresence [(Tag, BS.ByteString)] [XmlNode]
	deriving Show

data Query
	= IqBind (Maybe Requirement) Bind
	| IqSession
	| IqSessionNull
	| IqRoster (Maybe Roster)
	| QueryRaw [XmlNode]

	| IqCapsQuery BS.ByteString BS.ByteString
	| IqCapsQuery2 [XmlNode]
	| IqDiscoInfo
	| IqDiscoInfoNode [(DiscoTag, BS.ByteString)]
	| IqDiscoInfoFull [(DiscoTag, BS.ByteString)] [Identity] [InfoFeature]
		[XmlNode]
	deriving Show

data Bind
	= Resource BS.ByteString
	| BJid Jid
	| BindRaw XmlNode
	deriving Show

data IqType = Get | Set | Result | ITError deriving (Eq, Show)

data Roster = Roster (Maybe BS.ByteString) [XmlNode] deriving Show

data DiscoTag = DTNode | DTRaw QName deriving (Eq, Show)

data Identity
	= Identity [(IdentityTag, BS.ByteString)]
	| IdentityRaw XmlNode
	deriving Show

data IdentityTag
	= IDTType | IDTName | IDTCategory | IDTLang | IDTRaw QName deriving (Eq, Show)

data InfoFeature
	= InfoFeature BS.ByteString
	| InfoFeatureSemiRaw [(InfoFeatureTag, BS.ByteString)]
	| InfoFeatureRaw XmlNode
	deriving Show

data InfoFeatureTag = IFTVar | IFTVarRaw QName deriving (Eq, Show)

data Tag
	= Id | From | To | Version | Lang | Mechanism | Type
	| TagRaw QName
	deriving (Eq, Show)

toTag :: QName -> Tag
toTag ((_, Just "jabber:client"), "type") = Type
toTag ((_, Just "jabber:server"), "type") = Type
toTag ((_, Just "jabber:client"), "id") = Id
toTag ((_, Just "jabber:server"), "id") = Id
toTag ((_, Just "jabber:client"), "from") = From
toTag ((_, Just "jabber:server"), "from") = From
toTag ((_, Just "jabber:client"), "to") = To
toTag ((_, Just "jabber:server"), "to") = To
toTag ((_, Just "jabber:client"), "version") = Version
toTag ((_, Just "jabber:server"), "version") = Version
toTag ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism") = Mechanism
toTag (("xml", Nothing), "lang") = Lang
toTag n = TagRaw n

fromTag :: Tag -> QName
fromTag Type = nullQ "type"
fromTag Id = nullQ "id"
fromTag From = nullQ "from"
fromTag To = nullQ "to"
fromTag Version = nullQ "version"
fromTag Lang = (("xml", Nothing), "lang")
fromTag Mechanism = nullQ "mechanism"
fromTag (TagRaw n) = n

nullQ :: BS.ByteString -> QName
nullQ = (("", Nothing) ,)

data Requirement = Optional | Required | NoRequirement [XmlNode]
	deriving (Eq, Show)

toRequirement :: [XmlNode] -> Requirement
toRequirement [XmlNode (_, "optional") _ [] []] = Optional
toRequirement [XmlNode (_, "required") _ [] []] = Required
toRequirement n = NoRequirement n

fromRequirement :: Requirement -> XmlNode
fromRequirement Optional = XmlNode (nullQ "optional") [] [] []
fromRequirement Required = XmlNode (nullQ "required") [] [] []
fromRequirement (NoRequirement _) = undefined

data Feature
	= FtMechanisms [Mechanism]
	| FtStarttls Requirement
	| FtRosterver Requirement
	| FtBind Requirement
	| FtSession Requirement
	| FtRaw XmlNode
	deriving Show

toFeature :: XmlNode -> Feature
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls")
	_ [] r) = FtStarttls $ toRequirement r
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanisms")
	_ [] ns) = FtMechanisms $ map toMechanism ns
toFeature (XmlNode ((_, Just "urn:xmpp:features:rosterver"), "ver") _ [] r) =
	FtRosterver $ toRequirement r
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ [] r) =
	FtBind $ toRequirement r
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-session"), "session")
	_ [] r) = FtSession $ toRequirement r
toFeature n = FtRaw n

fromFeature :: Feature -> XmlNode
fromFeature (FtStarttls r) = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] [fromRequirement r]
fromFeature (FtMechanisms ms) = XmlNode (nullQ "mechanisms")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] $
	map fromMechanism ms
fromFeature (FtRosterver r) = XmlNode (nullQ "ver")
	[("", "urn:xmpp:features:rosterver")] [] [fromRequirement r]
fromFeature (FtBind r) = XmlNode (nullQ "bind")
	[("", "urn:ietf:params:xml:ns:xmpp-bind")] [] [fromRequirement r]
fromFeature (FtSession r) = XmlNode (nullQ "session")
	[("", "urn:ietf:params:xml:ns:xmpp-session")] [] [fromRequirement r]
fromFeature (FtRaw n) = n

data Mechanism
	= ScramSha1 | DigestMd5 | Plain | External | MechanismRaw BS.ByteString
	| McRaw XmlNode
	deriving (Eq, Show)

toMechanism' :: BS.ByteString -> Mechanism
toMechanism' "SCRAM-SHA1" = ScramSha1
toMechanism' "DIGEST-MD5" = DigestMd5
toMechanism' "PLAIN" = Plain
toMechanism' "EXTERNAL" = External
toMechanism' m = MechanismRaw m

fromMechanism' :: Mechanism -> BS.ByteString
fromMechanism' ScramSha1 = "SCRAM-SHA1"
fromMechanism' DigestMd5 = "DIGEST-MD5"
fromMechanism' Plain = "PLAIN"
fromMechanism' External = "EXTERNAL"
fromMechanism' (MechanismRaw m) = m
fromMechanism' (McRaw _) = error "fromMechanism': bad"

toMechanism :: XmlNode -> Mechanism
toMechanism (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism")
	_ [] [XmlCharData "SCRAM-SHA-1"]) = ScramSha1
toMechanism (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism")
	_ [] [XmlCharData "DIGEST-MD5"]) = DigestMd5
toMechanism (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism")
	_ [] [XmlCharData "PLAIN"]) = Plain
toMechanism (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism")
	_ [] [XmlCharData "EXTERNAL"]) = External
toMechanism (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism")
	_ [] [XmlCharData n]) = MechanismRaw n
toMechanism n = McRaw n

fromMechanism :: Mechanism -> XmlNode
fromMechanism (McRaw n) = n
fromMechanism m =
	XmlNode (nullQ "mechanism") [] [] [XmlCharData $ fromMechanism' m]

data MessageType = Normal | Chat | Groupchat | Headline | MTError
	deriving (Eq, Show)

toMessageType :: BS.ByteString -> MessageType
toMessageType "normal" = Normal
toMessageType "chat" = Chat
toMessageType _ = error "toMessageType: bad"

fromMessageType :: MessageType -> BS.ByteString
fromMessageType Normal = "normal"
fromMessageType Chat = "chat"
fromMessageType Groupchat = "groupchat"
fromMessageType Headline = "headline"
fromMessageType MTError = "error"

messageTypeToAtt :: MessageType -> (QName, BS.ByteString)
messageTypeToAtt = (nullQ "type" ,) . fromMessageType

data Jid = Jid BS.ByteString BS.ByteString (Maybe BS.ByteString) deriving (Eq, Show)

fromJid :: Jid -> BS.ByteString
fromJid (Jid a d r) = a `BS.append` "@" `BS.append` d `BS.append`
	maybe "" ("/" `BS.append`) r

toJid :: BS.ByteString -> Jid
toJid j = case rst of
	"" -> Jid "" a Nothing
	_ -> Jid a d (if BS.null r then Nothing else Just $ BS.tail r)
	where
	(a, rst) = BSC.span (/= '@') j
	(d, r) = BSC.span (/= '/') $ BS.tail rst

data MessageBody
	= MessageBody BS.ByteString
	| MBRaw XmlNode
	deriving Show

toBody :: XmlNode -> MessageBody
toBody (XmlNode ((_, Just "jabber:client"), "body") _ [] [XmlCharData b]) =
	MessageBody b
toBody (XmlNode ((_, Just "jabber:server"), "body") _ [] [XmlCharData b]) =
	MessageBody b
toBody n = MBRaw n

data MBody
	= MBody MessageBody
	| MBodyDelay MessageBody MessageDelay MessageXDelay
	| MBodyRaw [XmlNode]
	deriving Show

data MessageDelay
	= MessageDelay [(DelayTag, BS.ByteString)]
	| MDRaw XmlNode
	deriving Show

data DelayTag = DTFrom | DTStamp | DlyTRaw QName deriving Show

data MessageXDelay
	= MessageXDelay [(XDelayTag, BS.ByteString)]
	| MXDRaw XmlNode
	deriving Show

data XDelayTag = XDTFrom | XDTStamp | XDlyTRaw QName deriving Show

toXDelay :: XmlNode -> MessageXDelay
toXDelay (XmlNode ((_, Just "jabber:x:delay"), "x") _ as []) =
	MessageXDelay $ map (first toXDelayTag) as
toXDelay n = MXDRaw n

toXDelayTag :: QName -> XDelayTag
toXDelayTag ((_, Just "jabber:x:delay"), "from") = XDTFrom
toXDelayTag ((_, Just "jabber:x:delay"), "stamp") = XDTStamp
toXDelayTag n = XDlyTRaw n

toDelayTag :: QName -> DelayTag
toDelayTag ((_, Just "urn:xmpp:delay"), "from") = DTFrom
toDelayTag ((_, Just "urn:xmpp:delay"), "stamp") = DTStamp
toDelayTag n = DlyTRaw n

toDelay :: XmlNode -> MessageDelay
toDelay (XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ as []) = MessageDelay $
	map (first toDelayTag) as
toDelay n = MDRaw n

toCommon :: XmlNode -> Common
toCommon (XmlDecl (1, 0)) = XCDecl
toCommon (XmlStart ((_, Just "http://etherx.jabber.org/streams"), "stream") _ as) =
	XCBegin $ map (first toTag) as
toCommon (XmlEnd ((_, Just "http://etherx.jabber.org/streams"), "stream")) = XCEnd
toCommon (XmlNode ((_, Just "http://etherx.jabber.org/streams"), "features")
	_ [] nds) = XCFeatures $ map toFeature nds
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls")
	_ [] []) = XCStarttls
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed")
	_ [] []) = XCProceed
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth")
	_ [((_, "mechanism"), "EXTERNAL")] [XmlCharData "="]) = XCAuth External
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth")
	_ as [])
	| [(Mechanism, m)] <- map (first toTag) as =
		XCAuth $ toMechanism' m
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "success")
	_ [] []) = XCSaslSuccess

toCommon (XmlNode ((_, Just "jabber:client"), "message") _ as [b, d, xd])
	| XmlNode ((_, Just "jabber:client"), "body") _ [] _ <- b,
		XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ _ [] <- d,
		XmlNode ((_, Just "jabber:x:delay"), "x") _ _ [] <- xd =
		XCMessage tp i fr to $
			MBodyDelay (toBody b) (toDelay d) (toXDelay xd)
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts

toCommon (XmlNode ((_, Just "jabber:server"), "message") _ as [b, d, xd])
	| XmlNode ((_, Just "jabber:server"), "body") _ [] _ <- b,
		XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ _ [] <- d,
		XmlNode ((_, Just "jabber:x:delay"), "x") _ _ [] <- xd =
		XCMessage tp i fr to $
			MBodyDelay (toBody b) (toDelay d) (toXDelay xd)
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts

toCommon (XmlNode ((_, Just "jabber:server"), "message") _ as [b])
	| XmlNode ((_, Just "jabber:server"), "body") _ [] _ <- b =
		XCMessage tp i fr to $ MBody (toBody b)
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts

toCommon (XmlNode ((_, Just "jabber:client"), "message") _ as ns) =
	XCMessage tp i fr to $ MBodyRaw ns
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
	[] = filter ((`notElem` [Type, Id, From, To]) . fst) ts

toCommon (XmlNode ((_, Just "jabber:server"), "message") _ as ns) =
	XCMessage tp i fr to $ MBodyRaw ns
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
	[] = filter ((`notElem` [Type, Id, From, To]) . fst) ts

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

toCommon (XmlNode ((_, Just "jabber:client"), "iq") _ as ns) =
	SRIq tp i fr to $ toIqBody ns
	where
	ts = map (first toTag) as
	tp = toIqType . fromJust $ lookup Type ts
	Just i = lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid <$> lookup To ts

toCommon (XmlNode ((_, Just "jabber:server"), "iq") _ as ns) =
	SRIq tp i fr to $ toIqBody ns
	where
	ts = map (first toTag) as
	tp = toIqType . fromJust $ lookup Type ts
	Just i = lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid <$> lookup To ts
toCommon (XmlNode ((_, Just "jabber:client"), "presence") _ as ns) =
	SRPresence (map (first toTag) as) ns
toCommon (XmlNode ((_, Just "jabber:server"), "presence") _ as ns) =
	SRPresence (map (first toTag) as) ns

toCommon n = XCRaw n

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

fromJust' :: String -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' em _ = error em

resource :: BS.ByteString -> XmlNode
resource r = XmlNode (nullQ "resource") [] [] [XmlCharData r]

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

data Side = Client | Server deriving Show

jabberQ :: Side -> BS.ByteString
jabberQ Client = "jabber:client"
jabberQ Server = "jabber:server"

fromCommon :: Side -> Common -> XmlNode
fromCommon _ (XCDecl) = XmlDecl (1, 0)
fromCommon s (XCBegin ts) = XmlStart (("stream", Nothing), "stream")
	[	("", jabberQ s),
		("stream", "http://etherx.jabber.org/streams") ]
	(map (first fromTag) ts)
fromCommon _ XCEnd = XmlEnd (("stream", Nothing), "stream")
fromCommon _ (XCFeatures fs) =
	XmlNode (("stream", Nothing), "features") [] [] $ map fromFeature fs
fromCommon _ XCStarttls = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromCommon _ XCProceed = XmlNode (nullQ "proceed")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromCommon _ (XCAuth External) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[(nullQ "mechanism", "EXTERNAL")] [XmlCharData "="]
fromCommon _ (XCAuth m) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[(nullQ "mechanism", fromMechanism' m)] []
fromCommon _ XCSaslSuccess =
	XmlNode (nullQ "success") [("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon _ (XCMessage Chat i fr to (MBodyRaw ns)) =
	XmlNode (nullQ "message") [] (catMaybes [
		Just (fromTag Type, "chat"),
		Just (fromTag Id, i),
		(fromTag From ,) . fromJid <$> fr,
		Just (fromTag To, fromJid to) ]) ns

fromCommon _ SRChallengeNull = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon _ c@SRChallenge{} = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] $ fromChallenge
		(realm c) (nonce c) (qop c) (charset c) (algorithm c)
fromCommon _ (SRResponse _ dr) = drToXmlNode dr
fromCommon _ (SRChallengeRspauth sret) = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] [XmlCharData sret]
fromCommon _ SRResponseNull = drnToXmlNode
fromCommon _ (SRIq tp i fr to q) = XmlNode (nullQ "iq") []
	(catMaybes [
		Just $ iqTypeToAtt tp,
		Just (nullQ "id", i),
		(nullQ "from" ,) . fromJid <$> fr,
		(nullQ "to" ,) . fromJid <$> to ])
	(fromQuery q)
fromCommon _ (SRPresence ts c) =
	XmlNode (nullQ "presence") [] (map (first fromTag) ts) c
fromCommon _ (XCMessage tp i fr to (MBody (MessageBody m))) =
	XmlNode (nullQ "message") []
		(catMaybes [
			Just $ messageTypeToAtt tp,
			Just (nullQ "id", i),
			(nullQ "from" ,) . fromJid <$> fr,
			Just (nullQ "to", fromJid to) ])
		[XmlNode (nullQ "body") [] [] [XmlCharData m]]

fromCommon _ (XCRaw n) = n
fromCommon _ c = error $ "fromCommon: not implemented yet: " ++ show c

fromChallenge :: BS.ByteString -> BS.ByteString ->
	BS.ByteString -> BS.ByteString -> BS.ByteString -> [XmlNode]
fromChallenge r u q c a = (: []) . XmlCharData . B64.encode $ BS.concat [
	"realm=", BSC.pack $ show r, ",",
	"nonce=", BSC.pack $ show u, ",",
	"qop=", BSC.pack $ show q, ",",
	"charset=", c, ",", "algorithm=", a ] -- md5-sess" ]

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

capsQuery :: BS.ByteString -> BS.ByteString -> XmlNode
capsQuery v n = XmlNode (("", Nothing), "query")
	[("", "http://jabber.org/protocol/disco#info")]
	[((("", Nothing), "node"), n `BS.append` "#" `BS.append` v)] []

roster :: XmlNode
roster = XmlNode (nullQ "query") [("", "jabber:iq:roster")] [] []

session :: XmlNode
session = XmlNode (nullQ "session")
	[("", "urn:ietf:params:xml:ns:xmpp-session")] [] []
