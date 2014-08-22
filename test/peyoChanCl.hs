{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent hiding (yield)
import Data.HandleLike
import Data.Pipe
import Data.Pipe.ByteString
import System.IO
import Network
import Network.PeyoTLS.TChan.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

import ManyChanPipe

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	(g :: SystemRNG) <- cprgCreate <$> createEntropyPool
	h <- connectTo "localhost" $ PortNumber 4492
	(inc, otc) <- open' h "localhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca g
	pc <- atomically newTChan
	forkIO . void . runPipe $ fromChan inc
		=$= filterPipe (not . BS.null)
		=$= convert BS.init
		=$= filterPipe (not . BS.null)
		=$= convert BS.init
		=$= toChan pc
	sic <- atomically newTChan
	forkIO . void . runPipe $ fromHandleLn stdin =$= toChan sic
	_ <- runPipe $ fromChans [sic, pc]
		=$= debug
		=$= convert (`BS.append` "\n")
		=$= toChan otc
	return ()

debug :: Show a => Pipe a a IO ()
debug = await >>= maybe (return ()) (\x -> lift (print x) >> yield x >> debug)

filterPipe :: (a -> Bool) -> Pipe a a IO ()
filterPipe p = await >>= maybe (return ())
	(\x -> (if p x then yield x else return ()) >> filterPipe p)
