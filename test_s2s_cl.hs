{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.HandleLike
import System.IO
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import Text.XML.Pipe
import "crypto-random" Crypto.Random

import XmppClient

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 55269
	hlPutStrLn h $ xmlString begin
	hGetLine h >>= putStrLn
	hlPut h $ xmlString starttls
	hGetLine h >>= putStrLn
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`run` g) $ do
		p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca
		getNames p >>= liftIO . print
		hlPutStrLn p $ xmlString begin
		hlGetLine p >>= liftIO . print
		hlPutStrLn p $ xmlString auth
		hlGetLine p >>= liftIO . print
		hlPutStrLn p $ xmlString begin
		hlGetLine p >>= liftIO . print
		hlPutStrLn p $ xmlString message
		hlGetLine p >>= liftIO . print
		hlPutStrLn p $ xmlString [XmlEnd (("stream", Nothing), "stream")]
		hlClose p

begin :: [XmlNode]
begin = [
	XmlDecl (1, 0),
	XmlStart (("stream", Nothing), "stream")
		[	("", "jabber:server"),
			("stream", "http://etherx.jabber.org/streams") ]
		[	(nullQ "from", "localhost"),
			(nullQ "to", "otherhost"),
			(nullQ "version", "1.0") ]
	]

starttls :: [XmlNode]
starttls = [
	XmlNode (nullQ "starttls")
		[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
	]

auth :: [XmlNode]
auth = [
	XmlNode (nullQ "auth")
		[("", "urn:ietf:params:xml:ns:xmpp-sasl")]
		[(nullQ "mechanism", "EXTERNAL")]
		[XmlCharData "="]
	]

message :: [XmlNode]
message = [
	XmlNode (nullQ "message") []
		[	(nullQ "id", "ju2ba41c"),
			(nullQ "from", "yoshikuni@localhost"),
			(nullQ "to", "yoshio@otherhost"),
			(nullQ "type", "chat"),
			((("xml", Nothing), "lang"), "en") ]
		[XmlNode (nullQ "body") [] [] [XmlCharData
			"Art thou not Romeo, and a Montague?" ]]
	]
