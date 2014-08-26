{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent.STM

import System.Environment
import System.Random

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.Pipe.TChan
import Data.Pipe.IO (debug)
import Data.Pipe.ByteString
import Data.UUID
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.Sasl
import Network.PeyoTLS.TChan.Server
import qualified Network.PeyoTLS.TChan.Client as C
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

import Network.XMPiPe.Core.C2S.Server
import qualified Network.XMPiPe.Core.S2S.Client as SC
import Im

main :: IO ()
main = do
	(ip, _e) <- readFiles >>= \(ca, k, c) -> connect ca k c
--	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	pn : _ <- getArgs
	soc <- listenOn . PortNumber . fromIntegral $ (read :: String -> Int) pn
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`evalStateT` g0) . forever $ do
		(h, _, _) <- lift $ accept soc
		from <- lift $ atomically newTChan
		to <- lift $ atomically newTChan
		g <- StateT $ return . cprgFork
		(>> return ()) . liftBaseDiscard forkIO $ do
			do
				ss <- saslState . map toASCIIBytes . randoms <$>
					lift getStdGen
				(>> return ()) . liftIO . runPipe $
					fromHandle h =$= starttls =$= toHandle h
				(_cn, (inp, otp)) <- lift $
					open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
						[(k, c)] Nothing g
				Just ns <- (`evalStateT` ss) . runPipe $ do
					_ <- fromTChan inp =$= sasl =$= toTChan otp
					fromTChan inp =$= bind =@= toTChan otp
				_ <- liftBaseDiscard forkIO . (>> return ())
					. runPipe $ fromTChan inp
						=$= inputMpi ns
						=$= debug
						=$= toTChan from
				_ <- liftBaseDiscard forkIO . (>> return ())
					. runPipe $ fromTChan to
						=$= outputMpi =$= toTChan otp
				return ()
			uip <- lift $ atomically newTChan
			lift . atomically $ writeTChan uip sampleMessage
			(>> return ()) . runPipe $ fromTChans [from, uip]
				=$= makeP
				=$= outputSel
				=$= toTChans [(id, ip), (not, to)]

outputSel :: MonadIO m => Pipe Mpi (Bool, Mpi) m ()
outputSel = await >>= \mx -> case mx of
	Just m@(Message as _b)
		| Just (Jid "yoshio" "otherhost" Nothing) <- tagTo as ->
			yield (True, m) >> outputSel
	Just x -> yield (False, x) >> outputSel
	_ -> return ()

readFiles :: IO (CertificateStore, CertSecretKey, CertificateChain)
readFiles = (,,)
	<$> readCertificateStore ["certs/cacert.sample_pem"]
	<*> readKey "certs/localhost.sample_key"
	<*> readCertificateChain ["certs/localhost.sample_crt"]

sampleMessage :: Mpi
sampleMessage = let
	ts1 = (tagsType "chat") { tagFrom = Just sender, tagTo = Just reciever } in
	Message ts1 [XmlNode (nullQ "body") [] [] [XmlCharData "Hi, USER!"]]

makeP :: Monad m => Pipe Mpi Mpi m ()
makeP = await >>= \mm -> case mm of
	Just (Iq ts ns) |
		Just (IRRoster Nothing) <- toIRRoster ns,
		Just "get" <- tagType ts,
		Just i <- tagId ts -> do
		yield $ Iq (tagsType "result") { tagId = Just i }
			[fromIRRoster . IRRoster . Just $ Roster (Just "1") []]
		makeP
	Just (Presence _ _) -> makeP
	Just End -> yield End
	Just m -> yield m >> makeP
	_ -> return ()

sender, reciever :: Jid
sender = Jid "yoshio" "otherhost" (Just "profanity")
reciever = Jid "yoshikuni" "localhost" (Just "profanity")

connect :: CertificateStore -> CertSecretKey -> CertificateChain ->
	IO (TChan Mpi, TChan ())
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ fromHandle h =$= SC.starttls =$= toHandle h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(inc, otc) <- C.open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca g
		void . (`runStateT` St [] []) . runPipe $ fromTChan inc
			=$= SC.sasl =$= toTChan otc
		void . runPipe $ fromTChan inc =$= SC.begin =$= toTChan otc
		void . runPipe $ fromTChan i =$= SC.outputMpi =$= toTChan otc
	return (i, e)

data St = St {
	stFeatures :: [FeatureR],
	stSaslState :: [(BS.ByteString, BS.ByteString)] }
	deriving Show

instance SaslState St where
	getSaslState (St _ ss) = ss
	putSaslState ss (St fts _) = St fts ss

saslState :: [BS.ByteString] -> XmppState_
saslState uuids = XmppState_ {
	receiver = Nothing,
	uuidList = uuids,
	sState = [
		("realm", "localhost"),
		("nonce", "7658cddf-0e44-4de2-87df-4132bce97f4"),
		("qop", "auth"),
		("charset", "utf-8"),
		("algorithm", "md5-sess"),
		("snonce", "7658cddf-0e44-4de2-87df-4132bce97f4") ] }

data XmppState_ = XmppState_ {
	receiver :: Maybe Jid,
	uuidList :: [BS.ByteString],
	sState :: [(BS.ByteString, BS.ByteString)] }

instance XmppState XmppState_ where
	getXmppState xs = (receiver xs, uuidList xs)
	putXmppState (rcv, ul) xs = xs { receiver = rcv, uuidList = ul }

instance SaslState XmppState_ where
	getSaslState xs = case receiver xs of
		Just (Jid un _ _) -> ("username", un) : ss'
		_ -> ss'
		where
--		ss' = let u : _ = uuidList xs in ("uuid", toASCIIBytes u) : ss
		ss' = let u : _ = uuidList xs in ("uuid", u) : ss
		ss = sState xs
	putSaslState ss xs = case lookup "username" ss of
		Just un -> case receiver xs of
			Just (Jid _ d r) -> xs' { receiver = Just $ Jid un d r }
			_ -> xs' { receiver = Just $ Jid un "localhost" Nothing }
		_ -> xs'
		where
		xs' = xs {uuidList = tail $ uuidList xs, sState = ss}
