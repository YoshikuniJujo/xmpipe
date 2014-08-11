{-# LANGUAGE OverloadedStrings, PackageImports, FlexibleContexts #-}

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS

import SaslServer

exampleInit :: [(BS.ByteString, BS.ByteString)]
exampleInit = [
	("password", "password"),
	("realm", "localhost"),
	("nonce", "7658cddf-0e44-4de2-87df-41323bce97f4"),
	("qop", "auth"),
	("charset", "utf-8"),
	("algorithm", "md5-sess") ]

main :: IO ()
main = do
	r <- (`runStateT` ExampleState exampleInit) $ do
		runPipe $ fromFile "digestMd5cl.txt"
			=$= pipeSv digestMd5Sv
			=$= toStdout
	print r
