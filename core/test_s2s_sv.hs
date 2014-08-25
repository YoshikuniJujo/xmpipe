{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.Pipe.ByteString
import Data.UUID
import System.Random
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import S2sServer

instance SaslError Alert where
	fromSaslError et em = ExternalAlert $ show et ++ ":" ++ show em

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/otherhost.sample_key"
	c <- readCertificateChain ["certs/otherhost.sample_cert"]
	soc <- listenOn $ PortNumber 55269
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		sg <- newStdGen
		let us = randoms sg :: [UUID]
		void . forkIO . (`evalStateT` g0) $ do
			us' <- lift . (`execStateT` XmppState Nothing False us)
				. runPipe
				$ fromHandle h =$= starttls =$= toHandle h
			g <- StateT $ return . cprgFork
			liftIO . (`run` g) $ do
				p <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] (Just ca)
				getNames p >>= liftIO . print
				let sp = SHandle p
				us'' <- (`execStateT` us') . runPipe $
					fromHandleLike sp
						=$= sasl =$= toHandleLike sp
				void . (`evalStateT` us'')
					. runPipe $ fromHandleLike sp =$= inputP2
						=$= hlpDebug sp
						=$= process
						=$= outputS
						=$= toHandleLike sp

process :: (MonadState m, StateType m ~ XmppState) => Pipe Xmpp Xmpp m ()
process = await >>= \mx -> case mx of
	Just (XCBegin as) -> do
		modify $ \st -> st { xsDomainName = lookup From as }
		yield XCDecl
		nextUuid >>= yield . begin
		yield $ XCFeatures []
		process
	_ -> return ()
