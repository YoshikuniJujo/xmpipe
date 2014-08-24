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
import Control.Monad.Base
import Control.Concurrent (forkIO)
import Data.List
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

main :: IO ()
main = do
	(ip, e) <- readFiles >>= \(ca, k, c) -> connect ca k c
--	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	pn : _ <- getArgs
	soc <- listenOn . PortNumber . fromIntegral $ (read :: String -> Int) pn
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		from <- atomically newTChan
		to <- atomically newTChan
		(>> return ()) . forkIO $ do
			(`evalStateT` g0) $ do
				uuids <- randoms <$> lift getStdGen
				g <- StateT $ return . cprgFork
				(>> return ()) . liftIO . runPipe $
					fromHandleLike h
						=$= starttls =$= toHandleLike h
				(_cn, (inp, otp)) <- lift $
					open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
						[(k, c)] Nothing g
				(>> return ()) . lift
					. (`evalStateT` initXmppState uuids) $ do
					_ <- runPipe $ fromTChan inp
						=$= sasl =$= toTChan otp
					Just ns <- runPipe $ fromTChan inp
						=$= bind =@= toTChan otp
					_ <- liftBaseDiscard forkIO
						. (>> return ())
						. runPipe $ fromTChan inp
						=$= input ns
						=$= debug
						=$= toTChan from
					(>> return ()) . liftBaseDiscard forkIO
						. (>> return ())
						. runPipe $ fromTChan to
						=$= output
						=$= toTChan otp
			(>> return ()) . runPipe $ fromTChan from
				=$= makeP
				=$= outputSel'
				=$= toTChans [(id, ip), (not, to)]

toTChans :: MonadBase IO m => [(a -> Bool, TChan b)] -> Pipe (a, b) () m ()
toTChans cs = (await >>=) . maybe (return ()) $ \(t, x) -> (>> toTChans cs) $
	case find (($ t) . fst) cs of
		Just (_, c) -> lift . liftBase . atomically $ writeTChan c x
		_ -> return ()

outputSel' :: MonadIO m => Pipe Xmpp (Bool, Xmpp) m ()
outputSel' = await >>= \mx -> case mx of
	Just m@(SRMessage as _b)
		| Just (Jid "yoshio" "otherhost" Nothing) <- tagTo as ->
			yield (True, m) >> outputSel'
	Just x -> yield (False, x) >> outputSel'
	_ -> return ()

outputSel :: MonadIO m => TChan Xmpp -> TChan () -> Pipe Xmpp Xmpp m ()
outputSel ip e = await >>= \mx -> case mx of
	Just m@(SRMessage as _b)
		| Just (Jid "yoshio" "otherhost" Nothing) <- tagTo as -> do
			liftIO . atomically $ writeTChan ip m
			outputSel ip e
	Just x -> yield x >> outputSel ip e
	_ -> return ()

readFiles :: IO (CertificateStore, CertSecretKey, CertificateChain)
readFiles = (,,)
	<$> readCertificateStore ["certs/cacert.sample_pem"]
	<*> readKey "certs/localhost.sample_key"
	<*> readCertificateChain ["certs/localhost.sample_crt"]

makeP :: (
	MonadError m, SaslError (ErrorType m)) => Pipe Xmpp Xmpp m ()
makeP = (,) `liftM` await `ap` lift (return . Just $ Jid "yoshikuni" "localhost" (Just "profanity")) >>= \p -> case p of
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
