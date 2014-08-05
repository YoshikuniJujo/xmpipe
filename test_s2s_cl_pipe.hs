{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

import Debug.Trace

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import System.IO
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import Text.XML.Pipe
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 55269
	void . runPipe $ input h =$= processTls =$= output h
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	hGetChar h >>= print
	(`run` g) $ do
		p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca
		getNames p >>= liftIO . print
		void . runPipe $ input p =$= process =$= output p
		hlClose p

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

process :: Monad m => Pipe Xmpp Xmpp m ()
process = do
	yield XDecl
	yield begin
	proc

proc :: Monad m => Pipe Xmpp Xmpp m ()
proc = await >>= \mx -> case mx of
	Just (XBegin _as) -> procTls
	_ -> return ()

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = do
	yield XDecl
	yield begin
	procTls

procTls :: Monad m => Pipe Xmpp Xmpp m ()
procTls = await >>= \mx -> case mx of
	Just (XBegin _as) -> procTls
	Just (XFeatures [FtStarttls]) -> do
		yield XStarttls
		procTls
	Just XProceed -> return ()
	Just _ -> return ()
	_ -> return ()

begin :: Xmpp
begin = XBegin [
	(nullQ "from", "localhost"),
	(nullQ "to", "otherhost"),
	(nullQ "version", "1.0") ]

output :: HandleLike h => h -> Pipe Xmpp () (HandleMonad h) ()
output h = do
	mn <- await
	case mn of
		Just n -> do
			lift . hlPut h $ xmlString [fromXmpp n]
			case n of
				XEnd -> lift $ hlClose h
				_ -> return ()
			output h
		_ -> return ()

toXmpp :: XmlNode -> Xmpp
toXmpp (XmlDecl (1, 0)) = XDecl
toXmpp (XmlStart ((_, Just "http://etherx.jabber.org/streams"), "stream") _ as) =
	XBegin as
toXmpp (XmlEnd ((_, Just "http://etherx.jabber.org/streams"), "stream")) = XEnd
toXmpp (XmlNode ((_, Just "http://etherx.jabber.org/streams"), "features")
	_ [] ns) = XFeatures $ map toFeature ns
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls") _ [] []) =
	XStarttls
toXmpp (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed") _ [] []) =
	XProceed
toXmpp n = XRaw n

fromXmpp :: Xmpp -> XmlNode
fromXmpp XDecl = XmlDecl (1, 0)
fromXmpp (XBegin as) = XmlStart (("stream", Nothing), "stream")
	[	("", "jabber:server"),
		("stream", "http://etherx.jabber.org/streams") ] as
fromXmpp XEnd = XmlEnd (("stream", Nothing), "stream")
fromXmpp (XFeatures ns) =
	XmlNode (("stream", Nothing), "stream") [] [] $ map fromFeature ns
fromXmpp XStarttls = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp XProceed = XmlNode (nullQ "proceed")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
fromXmpp (XRaw n) = n

data Xmpp
	= XDecl
	| XBegin [(QName, BS.ByteString)]
	| XEnd
	| XFeatures [Feature]
	| XStarttls
	| XProceed
	| XRaw XmlNode
	deriving Show

data Feature
	= FtStarttls
	| FtMechanisms [XmlNode]
	| FtRaw XmlNode
	deriving Show

toFeature :: XmlNode -> Feature
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls")
	_ [] [XmlNode (_, "required") [] [] []]) = FtStarttls
toFeature (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-sasl"), "mechanisms")
	_ [] ns) = FtMechanisms ns
toFeature n = FtRaw n

fromFeature :: Feature -> XmlNode
fromFeature FtStarttls = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] []
	[XmlNode (nullQ "required") [] [] []]
fromFeature (FtMechanisms ns) = XmlNode (nullQ "mechanisms")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] ns
fromFeature (FtRaw n) = n

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

nullQ :: BS.ByteString -> QName
nullQ = (("", Nothing) ,)
