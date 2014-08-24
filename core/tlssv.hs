{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent.STM

import Data.UUID
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
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.PeyoTLS.TChan.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import XmppServer
import Im
import FederationClientIm

-- instance SaslError Alert where
--	fromSaslError et em = ExternalAlert $ show et ++ ":" ++ show em

main :: IO ()
main = do
	sl <- atomically $ newTVar []
--	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	pn : _ <- getArgs
	soc <- listenOn . PortNumber . fromIntegral $ (read :: String -> Int) pn
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		(>> return ()) . forkIO . (`evalStateT` g0) $ do
			uuids <- randoms <$> lift getStdGen
			g <- StateT $ return . cprgFork
			(>> return ()) . liftIO . runPipe $
				fromHandleLike h =$= starttls =$= toHandleLike h
			(_cn, (inp, otp)) <- lift $ open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
				[(k, c)] Nothing g
			from <- lift $ atomically newTChan
			to <- lift $ atomically newTChan
			(>> return ()) . lift
				. (`evalStateT` initXmppState uuids) $ do
				_ <- runPipe $
					fromTChan inp =$= sasl =$= toTChan otp
				Just ns <- runPipe $
					fromTChan inp =$= bind =@= toTChan otp
				_ <- liftBaseDiscard forkIO . (>> return ())
					. runPipe $ fromTChan inp
					=$= input ns
					=$= debug
					=$= toTChan from
				_ <- liftBaseDiscard forkIO . (>> return ())
					. runPipe $ fromTChan to
					=$= outputSel sl
					=$= output
					=$= toTChan otp
				(>> return ()) . runPipe $ fromTChan from
					=$= makeP
					=$= toTChan to

outputSel :: MonadIO m => TVar [(String, TChan Xmpp)] -> Pipe Xmpp Xmpp m ()
outputSel sl = await >>= \mx -> case mx of
	Just m@(SRMessage as _b)
		| Just (Jid "yoshio" "otherhost" Nothing) <- tagTo as -> do
			l <- liftIO (atomically $ readTVar sl)
			maybe (otherhost sl m)
				(liftIO . atomically . flip writeTChan m)
				$ lookup "otherhost" l
			outputSel sl
	Just x -> yield x >> outputSel sl
	_ -> return ()

otherhost :: MonadIO m =>
	TVar [(String, TChan Xmpp)] -> Xmpp -> Pipe Xmpp o m ()
otherhost sl m = liftIO $ do
	(ca, k, c) <- readFiles
	(ip, e) <- connect ca k c
	atomically $ writeTChan ip m
	atomically $ readTChan e
	atomically $ modifyTVar sl (("otherhost", ip) :)

readFiles :: IO (CertificateStore, CertSecretKey, CertificateChain)
readFiles = (,,)
	<$> readCertificateStore ["certs/cacert.sample_pem"]
	<*> readKey "certs/localhost.sample_key"
	<*> readCertificateChain ["certs/localhost.sample_crt"]

makeP :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m)) => Pipe Xmpp Xmpp m ()
makeP = (,) `liftM` await `ap` lift (gets receiver) >>= \p -> case p of
	(Just (XCBegin _), _) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, toASCIIBytes u),
			(From, "localhost"),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		yield . XCFeatures $ map featureRToFeature
			[FRRosterver Optional, Ft $ FtBind Required]
		makeP
	(Just (SRMessage _ bd), Just rcv) -> do
		let	ts1 = (tagsType "chat") {
				tagFrom = Just sender,
				tagTo = Just rcv }
			ts2 = (tagsType "chat") {
				tagFrom = Just $ Jid "yoshio" "otherhost" Nothing,
				tagTo = Just rcv }
			ts3 = (tagsType "chat") {
				tagFrom = Just $ Jid "yoshikuni" "localhost" $
					Just "profanity",
				tagTo = Just $ Jid "yoshio" "otherhost" Nothing }
		yield $ SRMessage ts1
			[XmlNode (nullQ "body") [] [] [XmlCharData "Hi, TLS!"]]
		yield $ SRMessage ts2 bd
		yield $ SRMessage ts3 bd
		makeP
	(Just (SRIq ts ns), _)
		| Just (IRRoster Nothing) <- toIRRoster ns,
			Just "get" <- tagType ts,
			Just i <- tagId ts -> do
			yield $ SRIq (tagsType "result") { tagId = Just i }
				[fromIRRoster . IRRoster . Just
					$ Roster (Just "1") []]
			makeP
	(Just (SRPresence _ _), Just rcv) -> do
		let	ts1 = Tags {
				tagId = Nothing,
				tagType = Just "chat",
				tagFrom = Just sender,
				tagTo = Just rcv,
				tagLang = Nothing,
				tagOthers = [] }
		yield $ SRMessage ts1
			[XmlNode (nullQ "body") [] [] [XmlCharData "Hi, TLS!"]]
		makeP
	_ -> return ()

sender :: Jid
sender = Jid "yoshio" "otherhost" (Just "profanity")
