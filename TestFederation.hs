{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module TestFederation (
	input, output, nullQ,
	Xmpp(..), toXmpp, fromXmpp, Feature(..), Mechanism(..),
	XmppCommon(..), Tag(..),
	Requirement(..),
	) where

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

input :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) ()
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

output :: HandleLike h => h -> Pipe Xmpp () (HandleMonad h) ()
output h = do
	mn <- await
	case mn of
		Just n -> do
			lift . hlPut h $ xmlString [fromXmpp n]
			case n of
				XCommon XCEnd -> lift $ hlClose h
				_ -> return ()
			output h
		_ -> return ()

toXmpp :: XmlNode -> Xmpp
toXmpp (XmlDecl (1, 0)) = XCommon XCDecl
toXmpp (XmlStart ((_, Just "http://etherx.jabber.org/streams"), "stream") _ as) =
	XCommon . XCBegin $ map (first toTag) as
toXmpp (XmlEnd ((_, Just "http://etherx.jabber.org/streams"), "stream")) =
	XCommon XCEnd
toXmpp (XmlNode ((_, Just "http://etherx.jabber.org/streams"), "features")
	_ [] ns) = XCommon . XCFeatures $ map toFeature ns
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls") _ [] []) =
	XCommon XCStarttls
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed") _ [] []) =
	XCommon XCProceed
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "auth") _
	[((_, "mechanism"), "EXTERNAL")] [XmlCharData "="]) =
	XCommon $ XCAuth External
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "success") _
	[] []) = XSuccess
toXmpp (XmlNode ((_, Just "jabber:server"), "message") [] as ns) = XMessage as ns
toXmpp n = XRaw n

fromXmpp :: Xmpp -> XmlNode
fromXmpp (XCommon XCDecl) = XmlDecl (1, 0)
fromXmpp (XCommon (XCBegin ts)) = XmlStart (("stream", Nothing), "stream")
	[	("", "jabber:server"),
		("stream", "http://etherx.jabber.org/streams") ] $
	map (first fromTag) ts
fromXmpp (XCommon XCEnd) = XmlEnd (("stream", Nothing), "stream")
fromXmpp (XCommon (XCFeatures ns)) =
	XmlNode (("stream", Nothing), "features") [] [] $ map fromFeature ns
fromXmpp (XCommon XCStarttls) = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp (XCommon XCProceed) = XmlNode (nullQ "proceed")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp (XCommon (XCAuth External)) = XmlNode (nullQ "auth")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
	[((nullQ "mechanism"), "EXTERNAL")] [XmlCharData "="]
fromXmpp XSuccess = XmlNode (nullQ "success")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []
fromXmpp (XMessage as ns) = XmlNode (nullQ "message") [] as ns
fromXmpp (XRaw n) = n

data Xmpp
	= XCommon XmppCommon
	| XSuccess
	| XMessage [(QName, BS.ByteString)] [XmlNode]
	| XRaw XmlNode
	deriving Show

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
