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
import Data.Pipe.TChan
import Data.Pipe.ByteString
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.PeyoTLS.TChan.Client
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
		(inc, otc) <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca g
		void . (`runStateT` St [] []) . runPipe $ fromTChan inc
			=$= sasl =$= toTChan otc
		void . runPipe $ fromTChan inc
			=$= inputP3
			=$= process i e
			=$= outputS
			=$= toTChan otc
	return (i, e)

process :: (
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
	MonadError m, Error (ErrorType m),
	MonadIO m) =>
	TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
proc ic e = await >>= \mx -> case mx of
	Just (XCBegin _as) -> proc ic e
	Just (XCFeatures []) -> federation
	Just (SRMessage{}) -> federation
	Just XCEnd -> yield XCEnd
	_ -> return ()
	where
	federation = do
		liftIO (atomically $ readTChan ic) >>= yield
		liftIO . atomically $ writeTChan e ()
		proc ic e
