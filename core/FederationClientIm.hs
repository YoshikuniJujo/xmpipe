{-# LANGUAGE OverloadedStrings, TupleSections, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module FederationClientIm (connect) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Pipe
import Data.Pipe.ByteString
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Client
import "crypto-random" Crypto.Random

import S2sClient

import Im

connect :: CertificateStore -> CertSecretKey -> CertificateChain ->
	IO (TChan Xmpp, TChan ())
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ fromHandle h =$= starttls =$= toHandle h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(`run` g) $ do
			p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] ca
			getNames p >>= liftIO . print
			let sp = SHandle p
			void . (`runStateT` St [] []) . runPipe $ fromHandleLike sp
				=$= inputP3
				=$= hlpDebug sp
				=$= process i e
				=$= outputS
				=$= toHandleLike sp
			void . (`runStateT` St [] []) . runPipe $ fromHandleLike sp
				=$= inputP3
				=$= hlpDebug sp
				=$= process i e
				=$= outputS
				=$= toHandleLike sp
			hlClose p
	return (i, e)

process :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m ) => TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
process i e = do
	yield XCDecl
	yield $ XCBegin [
		(From, "localhost"),
		(To, "otherhost"),
		(TagRaw $ nullQ "version", "1.0")]
	proc i e

proc :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m) =>
	TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
proc ic e = await >>= \mx -> case mx of
	Just (XCBegin _as) -> proc ic e
	Just (XCFeatures [FtMechanisms ["EXTERNAL"]]) -> do
		st <- lift $ gets getSaslState
		lift . modify . putSaslState $ ("username", "") : st
		sasl "EXTERNAL"
		lift . modify $ putSaslState st
	Just (XCFeatures []) -> federation
	Just (SRMessage{}) -> federation
	Just XCEnd -> yield XCEnd
	_ -> return ()
	where
	federation = do
		liftIO (atomically $ readTChan ic) >>= yield
		liftIO . atomically $ writeTChan e ()
		proc ic e
