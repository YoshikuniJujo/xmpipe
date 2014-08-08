{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module TestFederation (
	input, output, nullQ,
	Common(..), toXmpp, fromXmpp, Feature(..), Mechanism(..),
	XmppCommon(..), Tag(..),
	Requirement(..),
	MessageType(..),
	Jid(..),
	MBody(..),
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import XmppCommon

input :: HandleLike h => h -> Pipe () Common (HandleMonad h) ()
input h = handleP h
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlPipe
	=$= convert toXmpp
	=$= debugP h

debugP :: (HandleLike h, Show a) => h -> Pipe a a (HandleMonad h) ()
debugP h = await >>= \mx -> case mx of
	Just x -> do
		lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show x
		yield x
		debugP h
	_ -> return ()

output :: HandleLike h => h -> Pipe Common () (HandleMonad h) ()
output h = do
	mn <- await
	case mn of
		Just n -> do
			lift . hlPut h $ xmlString [fromXmpp n]
			case n of
				CCommon XCEnd -> lift $ hlClose h
				_ -> return ()
			output h
		_ -> return ()

toXmpp :: XmlNode -> Common
toXmpp (XmlDecl (1, 0)) = CCommon XCDecl
toXmpp (XmlStart ((_, Just "http://etherx.jabber.org/streams"), "stream") _ as) =
	CCommon . XCBegin $ map (first toTag) as
toXmpp (XmlEnd ((_, Just "http://etherx.jabber.org/streams"), "stream")) =
	CCommon XCEnd
toXmpp (XmlNode ((_, Just "http://etherx.jabber.org/streams"), "features")
	_ [] ns) = CCommon . XCFeatures $ map toFeature ns
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls") _ [] []) =
	CCommon XCStarttls
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed") _ [] []) =
	CCommon XCProceed
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth") _
	[((_, "mechanism"), "EXTERNAL")] [XmlCharData "="]) =
	CCommon $ XCAuth External
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "success") _
	[] []) = CCommon XCSaslSuccess
toXmpp (XmlNode ((_, Just "jabber:server"), "message") [] as ns) =
	CCommon . XCMessage tp i fr to $ MBodyRaw ns
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
	[] = filter ((`notElem` [Type, Id, From, To]) . fst) ts
toXmpp n = CCommon $ XCRaw n

fromXmpp :: Common -> XmlNode
fromXmpp (CCommon XCDecl) = XmlDecl (1, 0)
fromXmpp (CCommon (XCBegin ts)) = XmlStart (("stream", Nothing), "stream")
	[	("", "jabber:server"),
		("stream", "http://etherx.jabber.org/streams") ] $
	map (first fromTag) ts
fromXmpp (CCommon XCEnd) = XmlEnd (("stream", Nothing), "stream")
fromXmpp (CCommon (XCFeatures ns)) =
	XmlNode (("stream", Nothing), "features") [] [] $ map fromFeature ns
fromXmpp (CCommon XCStarttls) = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp (CCommon XCProceed) = XmlNode (nullQ "proceed")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp (CCommon (XCAuth External)) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[((nullQ "mechanism"), "EXTERNAL")] [XmlCharData "="]
fromXmpp (CCommon XCSaslSuccess) = XmlNode (nullQ "success")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromXmpp (CCommon (XCMessage tp i fr to (MBodyRaw ns))) =
	XmlNode (nullQ "message") [] (catMaybes [
		Just (fromTag Type, fromMessageType tp),
		Just (fromTag Id, i),
		(fromTag From ,) . fromJid <$> fr,
		Just (fromTag To, fromJid to) ]) ns
fromXmpp (CCommon (XCRaw n)) = n

handleP :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
handleP h = do
	c <- lift $ hlGetContent h
	yield c
	handleP h

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) (\x -> yield (f x) >> convert f)

xmlPipe :: Monad m => Pipe XmlEvent XmlNode m ()
xmlPipe = do
	c <- xmlBegin >>= xmlNode
	when c xmlPipe
