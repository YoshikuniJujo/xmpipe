{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TChan
import Data.Pipe
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
-- import System.IO
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import Text.XML.Pipe
import "crypto-random" Crypto.Random

import TestFederation

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	(i, e) <- connect ca k c
	atomically . writeTChan i $ XMessage
		[	(nullQ "type", "chat"),
			(nullQ "id", "some_id"),
			(nullQ "from", "yoshikuni@localhost"),
			(nullQ "to", "yoshio@otherhost"),
			((("xml", Nothing), "lang"), "en")
			]
		[XmlNode (nullQ "body") [] [] [XmlCharData
			"Art thou not Romeo, and a Montague or Hogeru?"]]
	atomically $ readTChan e

connect :: CertificateStore -> CertSecretKey -> CertificateChain -> IO (TChan Xmpp, TChan ())
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ input h =$= processTls =$= output h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	--	hGetChar h >>= print
		(`run` g) $ do
			p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] ca
			getNames p >>= liftIO . print
			void . runPipe $ input p =$= process i =$= output p
			hlClose p
			liftIO $ atomically $ writeTChan e ()
	return (i, e)

process :: MonadIO m => TChan Xmpp -> Pipe Xmpp Xmpp m ()
process i = do
	yield XDecl
	yield begin
	proc i

proc :: MonadIO m => TChan Xmpp -> Pipe Xmpp Xmpp m ()
proc i = await >>= \mx -> case mx of
	Just (XBegin _as) -> proc i
	Just (XFeatures [FtMechanisms [External]]) -> do
		yield XAuthExternal
		proc i
	Just XSuccess -> do
		yield XDecl
		yield begin
		proc i
	Just (XFeatures []) -> do
	{-
		yield $ XMessage
			[	(nullQ "type", "chat"),
				(nullQ "id", "some_id"),
				(nullQ "from", "yoshikuni@localhost"),
				(nullQ "to", "yoshio@otherhost"),
				((("xml", Nothing), "lang"), "en")
				]
			[XmlNode (nullQ "body") [] [] [XmlCharData
				"Art thou not Romeo, and a Montague?"]]
				-}
--		yield XEnd
		m <- liftIO . atomically $ readTChan i
		yield m
		proc i
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
