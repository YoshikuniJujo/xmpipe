{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module TestFederationCl (Xmpp, readFiles, convertMessage, connect) where

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Pipe
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import TestFederation
import Common hiding (nullQ, External)

convertMessage :: Common -> Xmpp
convertMessage (SRMessage Chat i (Just fr) to (MBody (MessageBody bd))) =
	XMessage
		[	(nullQ "type", "chat"),
			(nullQ "id", i),
			(nullQ "from", fromJid fr),
			(nullQ "to", fromJid to) ]
		[XmlNode (nullQ "body") [] [] [XmlCharData bd]]
convertMessage (SRMessage Chat i (Just fr) to (MBodyRaw ns)) =
	XMessage
		[	(nullQ "type", "chat"),
			(nullQ "id", i),
			(nullQ "from", fromJid fr),
			(nullQ "to", fromJid to) ] ns
convertMessage c = error $ "NOT IMPLEMENTED: " ++ show c

{-
sampleMessage :: Xmpp
sampleMessage = XMessage
	[	(nullQ "type", "chat"),
		(nullQ "id", "some_id"),
		(nullQ "from", "yoshikuni@localhost"),
		(nullQ "to", "yoshio@otherhost"),
		((("xml", Nothing), "lang"), "en") ]
	[XmlNode (nullQ "body") [] [] [XmlCharData
		"Art thou not Romeo, and a Montague or Hogeru?"]]
		-}

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
		void . runPipe $ input h =$= processTls =$= output h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(`run` g) $ do
			p <- open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] ca
			getNames p >>= liftIO . print
			void . runPipe $ input p =$= process i e =$= output p
			hlClose p
	return (i, e)

process :: MonadIO m => TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
process i e = do
	yield $ XCommon XCDecl
	yield begin
	proc i e

proc :: MonadIO m => TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
proc i e = await >>= \mx -> case mx of
	Just (XCommon (XCBegin _as)) -> proc i e
	Just (XCommon (XCFeatures [FtMechanisms [External]])) -> do
		yield . XCommon $ XCAuth External
		proc i e
	Just XSuccess -> do
		yield $ XCommon XCDecl
		yield begin
		proc i e
	Just (XCommon (XCFeatures [])) -> do
		m <- liftIO . atomically $ readTChan i
		yield m
		liftIO . atomically $ writeTChan e ()
		proc i e
	Just (XMessage _ _) -> do
		m <- liftIO . atomically $ readTChan i
		yield m
		liftIO . atomically $ writeTChan e ()
		proc i e
	Just (XCommon XCEnd) -> yield $ XCommon XCEnd
	_ -> return ()

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = do
	yield $ XCommon XCDecl
	yield begin
	procTls

procTls :: Monad m => Pipe Xmpp Xmpp m ()
procTls = await >>= \mx -> case mx of
	Just (XCommon (XCBegin _as)) -> procTls
	Just (XCommon (XCFeatures [FtStarttls _])) -> do
		yield $ XCommon XCStarttls
		procTls
	Just (XCommon XCProceed) -> return ()
	Just _ -> return ()
	_ -> return ()

begin :: Xmpp
begin = XCommon $ XCBegin [
	(From, "localhost"),
	(To, "otherhost"),
	(Version, "1.0") ]
