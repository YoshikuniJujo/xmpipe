{-# LANGUAGE OverloadedStrings, TupleSections, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module FederationClientIm (Im, connect) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Pipe
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
import Network
import Network.PeyoTLS.Client
import "crypto-random" Crypto.Random

import Tools
import SaslClient
import Xmpp
import Im

connect :: CertificateStore -> CertSecretKey -> CertificateChain ->
	IO (TChan (Either Im Xmpp), TChan ())
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ input h
			=$= hlpDebug h
			=$= processTls
			=$= output h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(`run` g) $ do
			p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] ca
			getNames p >>= liftIO . print
			let sp = SHandle p
			void . (`runStateT` St []) . runPipe $ input sp
				=$= hlpDebug sp
				=$= process i e
				=$= output sp
			void . (`runStateT` St []) . runPipe $ input sp
				=$= hlpDebug sp
				=$= process i e
				=$= output sp
			hlClose p
	return (i, e)

process :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m ) => TChan (Either Im Xmpp) -> TChan () -> Pipe Xmpp Xmpp m ()
process i e = do
	yield XCDecl
	yield $ XCBegin [(From, "localhost"), (To, "otherhost"), (Version, "1.0")]
	proc i e

proc :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m) =>
	TChan (Either Im Xmpp) -> TChan () -> Pipe Xmpp Xmpp m ()
proc ic e = await >>= \mx -> case mx of
	Just (XCBegin _as) -> proc ic e
	Just (XCFeatures [FtMechanisms ["EXTERNAL"]]) -> do
		st <- lift $ gets getSaslState
		lift . modify . putSaslState $ ("username", "") : st
		sasl "EXTERNAL"
		lift . modify $ putSaslState st
	Just (XCFeatures []) -> federation
	Just XCMessage{} -> federation
	Just XCEnd -> yield XCEnd
	_ -> return ()
	where
	federation = do
		em <- liftIO .atomically $ readTChan ic
		yield $ case em of
			Right x -> x
			Left (ImMessage tp i f t b) -> XCMessage tp i f t b
			_ -> error "bad"
		liftIO . atomically $ writeTChan e ()
		proc ic e

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = do
	yield XCDecl
	yield $ XCBegin [(From, "localhost"), (To, "otherhost"), (Version, "1.0")]
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
