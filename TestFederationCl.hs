{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module TestFederationCl (Common, readFiles, convertMessage, connect) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Pipe
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import TestFederation

convertMessage :: Common -> Common
convertMessage (XCMessage Chat i fr to mb) = XCMessage Chat i fr to mb
convertMessage c = error $ "NOT IMPLEMENTED: " ++ show c

readFiles :: IO (CertificateStore, CertSecretKey, CertificateChain)
readFiles = (,,)
	<$> readCertificateStore ["certs/cacert.sample_pem"]
	<*> readKey "certs/localhost.sample_key"
	<*> readCertificateChain ["certs/localhost.sample_crt"]

connect :: CertificateStore -> CertSecretKey -> CertificateChain -> IO (TChan Common, TChan ())
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ input h =$= processTls =$= output h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(`run` g) $ do
			p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] ca
			getNames p >>= liftIO . print
			void . runPipe $ input p =$= process i e =$= output p
			hlClose p
	return (i, e)

process :: MonadIO m => TChan Common -> TChan () -> Pipe Common Common m ()
process i e = yield XCDecl >> yield begin >> proc i e

proc :: MonadIO m => TChan Common -> TChan () -> Pipe Common Common m ()
proc i e = await >>= \mx -> case mx of
	Just (XCBegin _as) -> proc i e
	Just (XCFeatures [FtMechanisms [External]]) -> do
		yield $ XCAuth External
		proc i e
	Just XCSaslSuccess -> yield XCDecl >> yield begin >> proc i e
	Just (XCFeatures []) -> federation
	Just XCMessage{} -> federation
	Just XCEnd -> yield XCEnd
	_ -> return ()
	where
	federation = do
		m <- liftIO .atomically $ readTChan i
		yield m
		liftIO . atomically $ writeTChan e ()
		proc i e

processTls :: Monad m => Pipe Common Common m ()
processTls = do
	yield XCDecl
	yield begin
	procTls

procTls :: Monad m => Pipe Common Common m ()
procTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> procTls
	Just (XCFeatures [FtStarttls _]) -> do
		yield XCStarttls
		procTls
	Just XCProceed -> return ()
	Just _ -> return ()
	_ -> return ()

begin :: Common
begin = XCBegin [
	(From, "localhost"),
	(To, "otherhost"),
	(Version, "1.0") ]
