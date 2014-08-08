{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent
import Data.HandleLike
import System.IO
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import Text.XML.Pipe
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import XmppServer

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/otherhost.sample_key"
	c <- readCertificateChain ["certs/otherhost.sample_cert"]
	soc <- listenOn $ PortNumber 55269
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO . (`evalStateT` g0) $ do
			liftIO $ hGetTag h >>= BSC.hPutStrLn stdout
			liftIO $ hGetTag h >>= BSC.hPutStrLn stdout
			liftIO . hlPutStrLn h . xmlString $ begin ++ tlsFeatures
			liftIO $ hGetTag h >>= BSC.hPutStrLn stdout
			liftIO $ hGetTag h >>= BSC.hPutStrLn stdout
			liftIO . hlPutStrLn h $ xmlString proceed
			g <- StateT $ return . cprgFork
			liftIO . (`run` g) $ do
				p <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] (Just ca)
				getNames p >>= liftIO . print
				hGetTag p >>= liftIO . BSC.hPutStrLn stdout
				hGetTag p >>= liftIO . BSC.hPutStrLn stdout
				hlPutStrLn p . xmlString $ begin ++ externalFeatures
				hGetTag p >>= liftIO . print
				hGetTag p >>= liftIO . print
				hlPutStrLn p $ xmlString success
				hGetTag p >>= liftIO . print
				hGetTag p >>= liftIO . print
				hlPutStrLn p . xmlString $ begin ++ nullFeatures
				hGetTag p >>= liftIO . print
				hGetTag p >>= liftIO . print
				hGetTag p >>= liftIO . print
				hGetTag p >>= liftIO . print
				hlPutStrLn p $ xmlString
					[XmlEnd (("stream", Nothing), "stream")]
				hGetTag p >>= liftIO . print
--				hlClose p

hGetTag :: HandleLike h => h -> HandleMonad h BS.ByteString
hGetTag h = do
	c <- hlGet h 1
	if c == ">" then return ">" else (c `BS.append`) `liftM` hGetTag h

begin :: [XmlNode]
begin = [
	XmlDecl (1, 0),
	XmlStart (("stream", Nothing), "stream")
		[	("", "jabber:server"),
			("stream", "http://etherx.jabber.org/streams") ]
		[	(nullQ "id", "83e074ac-c014-432e9f21-d06e73f5777e"),
			(nullQ "from", "otherhost"),
			(nullQ "to", "localhost"),
			(nullQ "version", "1.0"),
			((("xml", Nothing), "lang"), "en") ]
	]

tlsFeatures :: [XmlNode]
tlsFeatures = [XmlNode (("stream", Nothing), "features") [] [] [starttls]]

starttls :: XmlNode
starttls = XmlNode (nullQ "starttls") [("", "urn:ietf:params:xml:ns:xmpp-tls")] []
	[XmlNode (nullQ "required") [] [] []]

proceed :: [XmlNode]
proceed = [
	XmlNode (nullQ "proceed")
		[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
	]

externalFeatures :: [XmlNode]
externalFeatures =
	[XmlNode (("stream", Nothing), "features") [] [] [externalMechanisms]]

externalMechanisms :: XmlNode
externalMechanisms = XmlNode (nullQ "mechanisms")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] [external]

external :: XmlNode
external = XmlNode (nullQ "mechanism") [] [] [XmlCharData "EXTERNAL"]

success :: [XmlNode]
success = [
	XmlNode (nullQ "success") [("", "urn:ietf:params:xml:ns:xmpp-sasl")] [] []]

nullFeatures :: [XmlNode]
nullFeatures = [XmlNode (("stream", Nothing), "features") [] [] []]
