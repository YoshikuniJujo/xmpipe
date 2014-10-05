{-# LANGUAGE OverloadedStrings, TupleSections #-}

module XmppType (
	Xmpp(..), toCommon, fromCommon,
	Tags(..),
	Jid(..), toJid, fromJid,
	Side(..),
	Tag(..), toTag, fromTag,
	Feature(..),
	Query(..), Bind(..),
	Requirement(..), toRequirement, fromRequirement,

	Mpi(..), toMpi, fromMpi,
	) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
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

	| SRMessage Tags [XmlNode]
	| SRPresence Tags [XmlNode]
	| SRIq Tags [XmlNode]
--	| SRIq [(Tag, BS.ByteString)] [XmlNode]

	| XCRaw XmlNode
	deriving (Eq, Show)

data Mpi
	= Message Tags [XmlNode]
	| Presence Tags [XmlNode]
	| Iq Tags [XmlNode]
	| End
	| MpiRaw XmlNode
	deriving (Eq, Show)

data Tags = Tags {
	tagId :: Maybe BS.ByteString,
	tagType :: Maybe BS.ByteString,
	tagFrom :: Maybe Jid,
	tagTo :: Maybe Jid,
	tagLang :: Maybe BS.ByteString,
	tagOthers :: [(QName, BS.ByteString)] } deriving (Eq, Show)

toTags :: [(QName, BS.ByteString)] -> Tags
toTags as = Tags {
	tagId = i, tagType = tp, tagFrom = fr, tagTo = to, tagLang = ln,
	tagOthers = ot }
	where
	ts = map (first toTag) as
	i = lookup Id ts
	tp = lookup Type ts
	fr = toJid <$> lookup From ts
	to = toJid <$> lookup To ts
	ln = lookup Lang ts
	ot = map (first $ \(TagRaw a) -> a) $ filter (isTagRaw . fst) ts

fromTags :: Tags -> [(QName, BS.ByteString)]
fromTags Tags { tagId = i, tagType = tp, tagFrom = fr, tagTo = to, tagLang = ln,
	tagOthers = ot } = (++ ot) . map (first fromTag) $ catMaybes [
		(Id ,) <$> i, (Type ,) <$> tp, (From ,) . fromJid <$> fr,
		(To ,) . fromJid <$> to, (Lang ,) <$> ln ]

data Query = IqBind (Maybe Requirement) Bind deriving (Eq, Show)

data Bind
	= Resource BS.ByteString
	| BJid Jid
	| BindRaw XmlNode
	deriving (Eq, Show)

data Tag
	= Id | Type | From | To | Lang | TagRaw QName
	deriving (Eq, Show)

isTagRaw :: Tag -> Bool
isTagRaw (TagRaw _) = True
isTagRaw _ = False

toTag :: QName -> Tag
toTag ((_, Just "jabber:client"), "id") = Id
toTag ((_, Just "jabber:server"), "id") = Id
toTag ((_, Just "jabber:client"), "type") = Type
toTag ((_, Just "jabber:server"), "type") = Type
toTag ((_, Just "jabber:client"), "from") = From
toTag ((_, Just "jabber:server"), "from") = From
toTag ((_, Just "jabber:client"), "to") = To
toTag ((_, Just "jabber:server"), "to") = To
toTag (("xml", Nothing), "lang") = Lang
toTag n = TagRaw n

fromTag :: Tag -> QName
fromTag Id = nullQ "id"
fromTag Type = nullQ "type"
fromTag From = nullQ "from"
fromTag To = nullQ "to"
fromTag Lang = (("xml", Nothing), "lang")
fromTag (TagRaw n) = n

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

data Jid = Jid BS.ByteString BS.ByteString (Maybe BS.ByteString) deriving (Eq, Show)

fromJid :: Jid -> BS.ByteString
fromJid (Jid a d r) = (if BS.null a then "" else a `BS.append` "@")
	`BS.append` d `BS.append` maybe "" ("/" `BS.append`) r

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
	| [(((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism"), m)] <- as =
	XCAuth m Nothing
-- toTag ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism") = Mechanism
toCommon (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth")
	_ as [XmlCharData i])
	| [(((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanism"), m)] <- as =
--	| [(Mechanism, m)] <- map (first toTag) as =
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
		SRMessage (toTags as) ns
toCommon (XmlNode ((_, Just q), "iq") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"], Just b <- toIqBody ns =
		SRIqBind ts b
	where ts = map (first toTag) as
toCommon (XmlNode ((_, Just q), "iq") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] = SRIq ts ns
	where ts = toTags as
toCommon (XmlNode ((_, Just "jabber:client"), "presence") _ as ns) =
	SRPresence (toTags as) ns
toCommon (XmlNode ((_, Just "jabber:server"), "presence") _ as ns) =
	SRPresence (toTags as) ns

toCommon n = XCRaw n

toMpi :: XmlNode -> Mpi
toMpi (XmlNode ((_, Just q), "message") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] = Message (toTags as) ns
toMpi (XmlNode ((_, Just q), "iq") _ as ns)
	| q `elem` ["jabber:client", "jabber:server"] = Iq ts ns
	where ts = toTags as
toMpi (XmlNode ((_, Just "jabber:client"), "presence") _ as ns) =
	Presence (toTags as) ns
toMpi (XmlNode ((_, Just "jabber:server"), "presence") _ as ns) =
	Presence (toTags as) ns
toMpi (XmlEnd ((_, Just "http://etherx.jabber.org/streams"), "stream")) = End
toMpi n = MpiRaw n

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
	XmlNode (nullQ "message") [] (fromTags ts) ns
fromCommon _ (SRIqBind ts q) = XmlNode (nullQ "iq") [] (map (first fromTag) ts)
	(fromQuery q)
fromCommon _ (SRIq ts q) = XmlNode (nullQ "iq") [] (fromTags ts) q
fromCommon _ (SRPresence ts c) = XmlNode (nullQ "presence") [] (fromTags ts) c
fromCommon _ (XCRaw n) = n

fromMpi :: Mpi -> XmlNode
fromMpi (Message ts ns) = XmlNode (nullQ "message") [] (fromTags ts) ns
fromMpi (Iq ts q) = XmlNode (nullQ "iq") [] (fromTags ts) q
fromMpi (Presence ts c) = XmlNode (nullQ "presence") [] (fromTags ts) c
fromMpi End = XmlEnd (("stream", Nothing), "stream")
fromMpi (MpiRaw n) = n

drToXmlNode :: BS.ByteString -> XmlNode
drToXmlNode dr = XmlNode (nullQ "response")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] []
	[XmlCharData $ B64.encode dr]

drnToXmlNode :: XmlNode
drnToXmlNode = XmlNode (nullQ "response")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []

fromQuery :: Query -> [XmlNode]
fromQuery (IqBind Nothing (BJid j)) = [
	XmlNode (nullQ "bind") [("", "urn:ietf:params:xml:ns:xmpp-bind")] []
		[XmlNode (nullQ "jid") [] [] [XmlCharData $ fromJid j]] ]
fromQuery (IqBind r b) = maybe id ((:) . fromRequirement) r $ fromBind b
