{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.Pipe.TChan
import Data.Pipe.IO (debug)
import Data.Pipe.ByteString
import Data.UUID
import System.Random
import Text.XML.Pipe
import Network
import Network.PeyoTLS.TChan.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Network.XMPiPe.Core.S2S.Server

import qualified Data.ByteString.Char8 as BSC

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
		let us = map toASCIIBytes (randoms sg :: [UUID])
		void . forkIO . (`evalStateT` g0) $ do
			us' <- lift . (`execStateT` XmppState Nothing us)
				. runPipe
				$ fromHandle h =$= starttls =$= toHandle h
			g <- StateT $ return . cprgFork
			(Just cn, (inp, otp)) <- lift $ open h
				["TLS_RSA_WITH_AES_128_CBC_SHA"] [(k, c)] (Just ca) g
			lift . print $ cn "localhost"
			us'' <- (`execStateT` us') . runPipe $ fromTChan inp
				=$= sasl (cn . BSC.unpack) =$= toTChan otp
			Just ns <- (`evalStateT` us'') . runPipe $
				fromTChan inp =$= begin =@= toTChan otp
			void . (`evalStateT` us'') . runPipe $ fromTChan inp
				=$= input ns
				=$= debug
				=$= process
				=$= output
				=$= toTChan otp

process :: (MonadState m, StateType m ~ XmppState) => Pipe Mpi Mpi m ()
process = await >>= \mx -> case mx of
	Just (Message _ _) -> do
		yield (Message (tagsType "chat") [XmlCharData "hage"])
		process
	Just _ -> process
	_ -> return ()
