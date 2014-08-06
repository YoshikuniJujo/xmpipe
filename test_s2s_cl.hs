{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import Data.HandleLike
-- import System.IO
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import Text.XML.Pipe
import "crypto-random" Crypto.Random

import TestFederation

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 55269
	void . runPipe $ input h =$= processTls =$= output h
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
--	hGetChar h >>= print
	(`run` g) $ do
		p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca
		getNames p >>= liftIO . print
		void . runPipe $ input p =$= process =$= output p
		hlClose p

process :: Monad m => Pipe Xmpp Xmpp m ()
process = do
	yield XDecl
	yield begin
	proc

proc :: Monad m => Pipe Xmpp Xmpp m ()
proc = await >>= \mx -> case mx of
	Just (XBegin _as) -> proc
	Just (XFeatures [FtMechanisms [External]]) -> do
		yield XAuthExternal
		proc
	Just XSuccess -> do
		yield XDecl
		yield begin
		proc
	Just (XFeatures []) -> do
		yield $ XMessage
			[	(nullQ "type", "chat"),
				(nullQ "id", "some_id"),
				(nullQ "from", "yoshikuni@localhost"),
				(nullQ "to", "yoshio@otherhost"),
				((("xml", Nothing), "lang"), "en")
				]
			[XmlNode (nullQ "body") [] [] [XmlCharData
				"Art thou not Romeo, and a Montague?"]]
--		yield XEnd
		proc
	Just XEnd -> yield XEnd
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
