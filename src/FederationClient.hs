{-# LANGUAGE OverloadedStrings, TupleSections, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module FederationClient (Xmpp, readFiles, connect) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Maybe
import Data.Pipe
import Data.Pipe.Basic
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Tools
import SaslClient

readFiles :: IO (CertificateStore, CertSecretKey, CertificateChain)
readFiles = (,,)
	<$> readCertificateStore ["certs/cacert.sample_pem"]
	<*> readKey "certs/localhost.sample_key"
	<*> readCertificateChain ["certs/localhost.sample_crt"]

connect :: CertificateStore -> CertSecretKey -> CertificateChain -> IO (TChan Xmpp, TChan ())
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ fromHandleLike h
			=$= xmlEvent
			=$= convert fromJust
			=$= xmlReborn
			=$= convert toCommon
			=$= hlpDebug h
			=$= processTls
			=$= output h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(`run` g) $ do
			p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] ca
			getNames p >>= liftIO . print
			let sp = SHandle p
			void . (`runStateT` St []) . runPipe $ fromHandleLike sp
				=$= xmlEvent
				=$= convert fromJust
				=$= xmlReborn
				=$= convert toCommon
				=$= hlpDebug sp
				=$= process i e
				=$= output sp
			hlClose p
	return (i, e)

output :: HandleLike h => h -> Pipe Xmpp () (HandleMonad h) ()
output h = (await >>=) . maybe (return ()) $ \n -> do
	lift (hlPut h $ xmlString [fromCommon Server n]) >> case n of
		XCEnd -> lift $ hlClose h
		_ -> output h

process :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m ) => TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
process i e = yield XCDecl >> yield begin >> proc i e

proc :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m) =>
	TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
proc i e = await >>= \mx -> case mx of
	Just (XCBegin _as) -> proc i e
	Just (XCFeatures [FtMechanisms ["EXTERNAL"]]) -> do
		st <- lift $ gets getSaslState
		lift . modify . putSaslState $ ("username", "") : st
		sasl "EXTERNAL"
		lift . modify $ putSaslState st
		yield XCDecl
		yield begin
		proc i e
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

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = do
	yield XCDecl
	yield begin
	procTls

procTls :: Monad m => Pipe Xmpp Xmpp m ()
procTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> procTls
	Just (XCFeatures [FtStarttls _]) -> do
		yield XCStarttls
		procTls
	Just XCProceed -> return ()
	Just _ -> return ()
	_ -> return ()

begin :: Xmpp
begin = XCBegin [
	(From, "localhost"),
	(To, "otherhost"),
	(Version, "1.0") ]
