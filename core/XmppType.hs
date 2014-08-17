{-# LANGUAGE OverloadedStrings, TupleSections #-}

module XmppType (
	Xmpp(..), toCommon, fromCommon,

	Jid(..), toJid,
	Side(..),
	Feature(..),
	Tag(..), Requirement(..), toRequirement, fromRequirement,

	Bind(..), Roster(..),

	IqType(..), Query(..),
	MessageType(..), MBody(..),
	) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

data Xmpp
	= XCDecl
	| XCBegin [(Tag, BS.ByteString)]
	| XCEnd
	| XCFeatures [Feature]
	| XCStarttls
	| XCProceed
	| XCMessage MessageType BS.ByteString (Maybe Jid) Jid MBody
	| XCRaw XmlNode

	| XCAuth BS.ByteString (Maybe BS.ByteString)
	| SRChallenge BS.ByteString
	| SRResponse BS.ByteString
	| XCSaslSuccess (Maybe BS.ByteString)

	| SRIq IqType BS.ByteString (Maybe Jid) (Maybe Jid) Query
	| SRPresence [(Tag, BS.ByteString)] [XmlNode]
	deriving Show

data Query
	= IqBind (Maybe Requirement) Bind
	| IqRoster (Maybe Roster)
	| QueryNull
	| QueryRaw [XmlNode]
	deriving Show

data Bind
	= Resource BS.ByteString
	| BJid Jid
	| BindRaw XmlNode
	deriving Show

data IqType = Get | Set | Result | ITError deriving (Eq, Show)

data Roster = Roster (Maybe BS.ByteString) [XmlNode] deriving Show

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
	deriving (Eq, Ord, Show)

toRequirement :: [XmlNode] -> Requirement
toRequirement [XmlNode (_, "optional") _ [] []] = Optional
toRequirement [XmlNode (_, "required") _ [] []] = Required
toRequirement n = NoRequirement n

fromRequirement :: Requirement -> XmlNode
fromRequirement Optional = XmlNode (nullQ "optional") [] [] []
fromRequirement Required = XmlNode (nullQ "required") [] [] []
fromRequirement (NoRequirement _) = undefined

data Feature
	= FtMechanisms [BS.ByteString]
	| FtStarttls Requirement
	| FtBind Requirement
	| FtSession Requirement
	| FtRaw XmlNode
	deriving (Eq, Ord, Show)

toFeature :: XmlNode -> Feature
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls")
	_ [] r) = FtStarttls $ toRequirement r
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanisms")
	_ [] ns) = FtMechanisms $ map toMechanism ns
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
fromFeature (FtBind r) = XmlNode (nullQ "bind")
	[("", "urn:ietf:params:xml:ns:xmpp-bind")] [] [fromRequirement r]
fromFeature (FtSession r) = XmlNode (nullQ "session")
	[("", "urn:ietf:params:xml:ns:xmpp-session")] [] [fromRequirement r]
fromFeature (FtRaw n) = n

toMechanism :: XmlNode -> BS.ByteString
toMechanism (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism")
	_ [] [XmlCharData m]) = m
toMechanism _ = error "toMechanism: bad"

fromMechanism :: BS.ByteString -> XmlNode
fromMechanism m = XmlNode (nullQ "mechanism") [] [] [XmlCharData m]

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

data MBody
	= MBody BS.ByteString
	| MBodyRaw [XmlNode]
	deriving Show

toBody :: [XmlNode] -> MBody
toBody [XmlNode ((_, Just q), "body") _ [] [XmlCharData b]]
	| q `elem` ["jabber:client", "jabber:server"] = MBody b
toBody ns = MBodyRaw ns

toCommon :: XmlNode -> Xmpp
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
	_ as [])
	| [(Mechanism, m)] <- map (first toTag) as = XCAuth m Nothing
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth")
	_ as [XmlCharData i])
	| [(Mechanism, m)] <- map (first toTag) as =
	XCAuth m . Just . (\(Right r) -> r) $ case i of
		"=" -> Right ""
		_ -> B64.decode i
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "success")
	_ [] []) = XCSaslSuccess Nothing
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "success")
	_ [] [XmlCharData d]) =
	XCSaslSuccess . Just . (\(Right r) -> r) $ B64.decode d
toCommon (XmlNode ((_, Just q), "message") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] =
		XCMessage tp i fr to $ toBody ns
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
	[] = filter ((`notElem` [Type, Id, From, To]) . fst) ts

toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "challenge")
	_ [] []) = SRChallenge ""
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "challenge")
	_ [] [XmlCharData c]) = let Right d = B64.decode c in SRChallenge d
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "response")
	_ [] [XmlCharData cd]) = let Right s = B64.decode cd in SRResponse s
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "response")
	_ [] []) = SRResponse ""

toCommon (XmlNode ((_, Just q), "iq") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] =
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
toIqBody [XmlNode ((_, Just "jabber:iq:roster"), "query") _ [] []] =
	IqRoster Nothing
toIqBody [XmlNode ((_, Just "jabber:iq:roster"), "query") _ as ns] = IqRoster
	. Just $ Roster (snd <$> find (\((_, v), _) -> v == "ver") as) ns
toIqBody [] = QueryNull
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

fromCommon :: Side -> Xmpp -> XmlNode
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
fromCommon _ (XCAuth m Nothing) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[(nullQ "mechanism", m)] []
fromCommon _ (XCAuth m (Just i)) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[(nullQ "mechanism", m)]
	[XmlCharData $ case i of "" -> "="; _ -> B64.encode i]
fromCommon _ (XCSaslSuccess Nothing) =
	XmlNode (nullQ "success") [("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon _ (XCSaslSuccess (Just d)) =
	XmlNode (nullQ "success") [("", "urn:ietf:params:xml:ns:xmpp-sasl")] []
	[XmlCharData $ B64.encode d]
fromCommon _ (XCMessage Chat i fr to (MBodyRaw ns)) =
	XmlNode (nullQ "message") [] (catMaybes [
		Just (fromTag Type, "chat"),
		Just (fromTag Id, i),
		(fromTag From ,) . fromJid <$> fr,
		Just (fromTag To, fromJid to) ]) ns

fromCommon _ (SRChallenge "") = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon _ (SRChallenge c) = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] .
		(: []) . XmlCharData $ B64.encode c
fromCommon _ (SRResponse "") = drnToXmlNode
fromCommon _ (SRResponse s) = drToXmlNode s
fromCommon _ (SRIq tp i fr to q) = XmlNode (nullQ "iq") []
	(catMaybes [
		Just $ iqTypeToAtt tp,
		Just (nullQ "id", i),
		(nullQ "from" ,) . fromJid <$> fr,
		(nullQ "to" ,) . fromJid <$> to ])
	(fromQuery q)
fromCommon _ (SRPresence ts c) =
	XmlNode (nullQ "presence") [] (map (first fromTag) ts) c
fromCommon _ (XCMessage tp i fr to (MBody m)) =
	XmlNode (nullQ "message") []
		(catMaybes [
			Just $ messageTypeToAtt tp,
			Just (nullQ "id", i),
			(nullQ "from" ,) . fromJid <$> fr,
			Just (nullQ "to", fromJid to) ])
		[XmlNode (nullQ "body") [] [] [XmlCharData m]]

fromCommon _ (XCRaw n) = n
fromCommon _ c = error $ "fromCommon: not implemented yet: " ++ show c

drToXmlNode :: BS.ByteString -> XmlNode
drToXmlNode dr = XmlNode (nullQ "response")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] []
	[XmlCharData $ B64.encode dr]

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
fromQuery (IqRoster Nothing) = [roster]
fromQuery QueryNull = []
fromQuery (QueryRaw ns) = ns

roster :: XmlNode
roster = XmlNode (nullQ "query") [("", "jabber:iq:roster")] [] []
