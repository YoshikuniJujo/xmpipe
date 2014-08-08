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

fromChallenge :: BS.ByteString -> BS.ByteString ->
	BS.ByteString -> BS.ByteString -> BS.ByteString -> [XmlNode]
fromChallenge r u q c a = (: []) . XmlCharData . B64.encode $ BS.concat [
	"realm=", BSC.pack $ show r, ",",
	"nonce=", BSC.pack $ show u, ",",
	"qop=", BSC.pack $ show q, ",",
	"charset=", c, ",", "algorithm=", a ] -- md5-sess" ]

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
