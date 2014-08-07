{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Exception
import System.IO
import Network

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 54492
	hPutStrLn h "HELLO"
	hPutStrLn h "WORLD"
	threadDelay 500000
	hPutStrLn h "WORLD" `catch` \(e :: IOException) -> do
		h <- connectTo "localhost" $ PortNumber 54492
		hPutStrLn h "WORLD"
