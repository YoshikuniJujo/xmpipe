{-# LANGUAGE OverloadedStrings, TupleSections #-}

module XmppCommon (
	XmppCommon(..),
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
	) where

import Control.Arrow
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Digest

data Common
	= CCommon XmppCommon

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

data XmppCommon
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
