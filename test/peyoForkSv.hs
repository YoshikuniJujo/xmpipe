{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Data.HandleLike
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString.Char8 as BSC

voidM :: IO a -> IO ()
voidM = (>> return ())

count :: Int -> TChan Int -> IO ()
count n c = atomically (writeTChan c n) >> threadDelay 1000000 >> count (n + 1) c

main :: IO ()
main = do
	ch <- atomically newTChan
	forkIO $ count 0 ch
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	soc <- listenOn $ PortNumber 4492
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		voidM . forkIO . (`evalStateT` g0) $ do
			g <- StateT $ return . cprgFork
			liftIO . (`run` g) $ do
				p <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] Nothing
				forever $ do
					ln <- hlGetLine p
					liftIO $ print ln
					hlPutStrLn p ln
				hlClose p
