{-# LANGUAGE OverloadedStrings, TupleSections #-}

module XmppType (
	Xmpp(..), toCommon, fromCommon,

	Jid(..), toJid,
	Side(..),
	Feature(..),
	Tag(..), Requirement(..), toRequirement, fromRequirement,

	Bind(..), Roster(..),

	Query(..),
	MessageType(..),

	toTag, toMessageType, fromTag, fromJid,
	messageTypeToAtt,
	) where

import Control.Arrow
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

	| XCAuth BS.ByteString (Maybe BS.ByteString)
	| SRChallenge BS.ByteString
	| SRResponse BS.ByteString
	| XCSaslSuccess (Maybe BS.ByteString)

	| SRIqBind [(Tag, BS.ByteString)] Query

	| SRMessage [(Tag, BS.ByteString)] [XmlNode]
	| SRPresence [(Tag, BS.ByteString)] [XmlNode]
	| SRIq [(Tag, BS.ByteString)] [XmlNode]

	| XCRaw XmlNode
	deriving Show

data Query = IqBind (Maybe Requirement) Bind
	deriving Show

data Bind
	= Resource BS.ByteString
	| BJid Jid
	| BindRaw XmlNode
	deriving Show

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
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "challenge")
	_ [] []) = SRChallenge ""
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "challenge")
	_ [] [XmlCharData c]) = let Right d = B64.decode c in SRChallenge d
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "response")
	_ [] [XmlCharData cd]) = let Right s = B64.decode cd in SRResponse s
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "response")
	_ [] []) = SRResponse ""

toCommon (XmlNode ((_, Just q), "message") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] =
		SRMessage (map (first toTag) as) ns
toCommon (XmlNode ((_, Just q), "iq") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"], Just b <- toIqBody ns =
		SRIqBind ts b
	where ts = map (first toTag) as
toCommon (XmlNode ((_, Just q), "iq") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] = SRIq ts ns
	where ts = map (first toTag) as
toCommon (XmlNode ((_, Just "jabber:client"), "presence") _ as ns) =
	SRPresence (map (first toTag) as) ns
toCommon (XmlNode ((_, Just "jabber:server"), "presence") _ as ns) =
	SRPresence (map (first toTag) as) ns

toCommon n = XCRaw n

toIqBody :: [XmlNode] -> Maybe Query
toIqBody [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ []
	[n]] = Just . IqBind Nothing $ toBind n
toIqBody [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ []
	[n, n']] | r <- toRequirement [n] = Just . IqBind (Just r) $ toBind n'
toIqBody _ = Nothing

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
fromCommon _ (SRChallenge "") = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromCommon _ (SRChallenge c) = XmlNode (nullQ "challenge")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] .
		(: []) . XmlCharData $ B64.encode c
fromCommon _ (SRResponse "") = drnToXmlNode
fromCommon _ (SRResponse s) = drToXmlNode s
fromCommon _ (SRMessage ts ns) =
	XmlNode (nullQ "message") [] (map (first fromTag) ts) ns
fromCommon _ (SRIqBind ts q) = XmlNode (nullQ "iq") [] (map (first fromTag) ts)
	(fromQuery q)
fromCommon _ (SRIq ts q) = XmlNode (nullQ "iq") [] (map (first fromTag) ts) q
fromCommon _ (SRPresence ts c) =
	XmlNode (nullQ "presence") [] (map (first fromTag) ts) c
fromCommon _ (XCRaw n) = n

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
fromQuery (IqBind r b) = maybe id ((:) . fromRequirement) r $ fromBind b
