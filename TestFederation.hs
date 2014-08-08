{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module TestFederation (
	input, output, nullQ,
	Common(..), toCommon, fromXmpp, Feature(..), Mechanism(..),
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
	=$= convert toCommon
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
			lift . hlPut h $ xmlString [fromXmpp Server n]
			case n of
				CCommon XCEnd -> lift $ hlClose h
				_ -> return ()
			output h
		_ -> return ()

data Side = Client | Server deriving Show

jabberQ :: Side -> BS.ByteString
jabberQ Client = "jabber:client"
jabberQ Server = "jabber:server"

fromXmpp :: Side -> Common -> XmlNode
fromXmpp _ (CCommon XCDecl) = XmlDecl (1, 0)
fromXmpp s (CCommon (XCBegin ts)) = XmlStart (("stream", Nothing), "stream")
	[	("", jabberQ s),
		("stream", "http://etherx.jabber.org/streams") ] $
	map (first fromTag) ts
fromXmpp _ (CCommon XCEnd) = XmlEnd (("stream", Nothing), "stream")
fromXmpp _ (CCommon (XCFeatures fs)) =
	XmlNode (("stream", Nothing), "features") [] [] $ map fromFeature fs
fromXmpp _ (CCommon XCStarttls) = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp _ (CCommon XCProceed) = XmlNode (nullQ "proceed")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp _ (CCommon (XCAuth External)) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[(nullQ "mechanism", "EXTERNAL")] [XmlCharData "="]
fromXmpp _ (CCommon (XCAuth m)) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[(nullQ "mechanism", fromMechanism' m)] []
fromXmpp _ (CCommon XCSaslSuccess) =
	XmlNode (nullQ "success") [("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromXmpp _ (CCommon (XCMessage tp i fr to (MBodyRaw ns))) =
	XmlNode (nullQ "message") [] (catMaybes [
		Just (fromTag Type, fromMessageType tp),
		Just (fromTag Id, i),
		(fromTag From ,) . fromJid <$> fr,
		Just (fromTag To, fromJid to) ]) ns

fromXmpp _ (CCommon (XCRaw n)) = n

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
