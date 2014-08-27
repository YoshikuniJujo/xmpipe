{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent.STM

import System.Environment
import System.Random

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Monad.Trans.Control
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.Pipe.TChan
import Data.Pipe.IO (debug)
import Data.Pipe.ByteString
import Data.UUID
import Data.X509
import Data.X509.CertificateStore
import System.IO
import Text.XML.Pipe
import Network
import Network.Sasl
import Network.PeyoTLS.TChan.Server
import qualified Network.PeyoTLS.TChan.Client as C
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.XMPiPe.Core.C2S.Server
import qualified Network.XMPiPe.Core.S2S.Client as SC
import Im
import Retrieve

main :: IO ()
main = do
	(ip, _e, op) <- readFiles >>= \(ca, k, c) -> connect ca k c
	_ <- forkIO . void . (runPipe :: Pipe () () IO () -> IO (Maybe ())) $
		fromTChan op =$= convert (BSC.pack . show) =$= toHandle stdout
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
			us <- map toASCIIBytes . randoms <$>
				lift getStdGen
			us' <- (`execStateT` us) . runPipe $
				fromHandle h =$= starttls "localhost" =$= toHandle h
			let ss = saslState (Jid "" "localhost" Nothing) us'
			(_cn, (inp, otp)) <- lift $
				open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] Nothing g
			Just ns <- (`evalStateT` ss) . runPipe $ do
				_ <- fromTChan inp
					=$= sasl "localhost" sampleRetrieves
					=$= toTChan otp
				fromTChan inp
					=$= bind "localhost" [featureRToNode $
						FRRosterver Optional]
					=@= toTChan otp
			_ <- liftBaseDiscard forkIO . (>> return ()) . runPipe $
				fromTChan inp
					=$= input ns =$= debug =$= toTChan from
			_ <- liftBaseDiscard forkIO . (>> return ()) . runPipe $
				fromTChan to =$= output =$= toTChan otp
			uip <- lift $ atomically newTChan
			lift . atomically $ writeTChan uip sampleMessage
			(>> return ()) . runPipe $
				fromTChans [from, uip]
					=$= makeP =$= outputSel
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
	IO (TChan Mpi, TChan (), TChan Mpi)
connect ca k c = do
	i <- atomically newTChan
	e <- atomically newTChan
	o <- atomically newTChan
	_ <- forkIO $ do
		h <- connectTo "localhost" $ PortNumber 55269
		void . runPipe $ fromHandle h
			=$= SC.starttls "localhost" "otherhost"
			=$= toHandle h
		g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
		(inc, otc) <- C.open' h "otherhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca g
		void . (`runStateT` St [] []) . runPipe $ fromTChan inc
			=$= SC.sasl "localhost" "otherhost"
			=$= toTChan otc
		Just ns <- runPipe $ fromTChan inc
			=$= SC.begin "localhost" "otherhost"
			=@= toTChan otc
		void . forkIO $ void . runPipe $
			fromTChan inc =$= SC.input ns =$= toTChan o
		void . runPipe $ fromTChan i =$= SC.output =$= toTChan otc
	return (i, e, o)

data St = St {
	stFeatures :: [FeatureR],
	stSaslState :: [(BS.ByteString, BS.ByteString)] }
	deriving Show

instance SaslState St where
	getSaslState (St _ ss) = ss
	putSaslState ss (St fts _) = St fts ss

saslState :: Jid -> [BS.ByteString] -> XmppState_
saslState jid uuids = XmppState_ {
	receiver = jid,
	uuidList = uuids,
	sState = [
		("realm", "localhost"),
		("nonce", "7658cddf-0e44-4de2-87df-4132bce97f4"),
		("qop", "auth"),
		("charset", "utf-8"),
		("algorithm", "md5-sess"),
		("snonce", "7658cddf-0e44-4de2-87df-4132bce97f4") ] }

data XmppState_ = XmppState_ {
	receiver :: Jid,
	uuidList :: [BS.ByteString],
	sState :: [(BS.ByteString, BS.ByteString)] }

instance XmppState XmppState_ where
	getXmppState xs = (receiver xs, uuidList xs)
	putXmppState (rcv, ul) xs = xs { receiver = rcv, uuidList = ul }

instance SaslState XmppState_ where
	getSaslState xs = case receiver xs of
		(Jid un _ _) -> ("username", un) : ss'
		where
		ss' = let u : _ = uuidList xs in ("uuid", u) : ss
		ss = sState xs
	putSaslState ss xs = case lookup "username" ss of
		Just un -> case receiver xs of
			Jid _ d r -> xs' { receiver = Jid un d r }
		_ -> xs'
		where
		xs' = xs {uuidList = tail $ uuidList xs, sState = ss}

sampleRetrieves :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m)) => [Retrieve m]
sampleRetrieves = [
	RTPlain retrievePln, RTExternal retrieveEx,
	RTDigestMd5 retrieveDM5, RTScramSha1 retrieveSS1 ]
