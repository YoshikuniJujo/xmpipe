{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent
import Data.HandleLike
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

main :: IO ()
main = do
	c <- atomically newTChan
	forkIO . forever $ BS.getLine >>= atomically . writeTChan c
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	(g :: SystemRNG) <- cprgCreate <$> createEntropyPool
	h <- connectTo "localhost" $ PortNumber 4492
	(`run` g) $ do
		p <- open' h "localhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		forever $ do
			hlPutStrLn p =<< liftIO (atomically $ readTChan c)
			hlGetLine p >>= liftIO . print
