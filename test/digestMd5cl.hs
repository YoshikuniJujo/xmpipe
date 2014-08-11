{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS

import SaslClient

exampleInit :: [(BS.ByteString, BS.ByteString)]
exampleInit = [
	("password", "password"),
	("username", "yoshikuni")
	]

main :: IO ()
main = do
	r <- (`runStateT` ExampleState exampleInit) $ do
		runPipe $ fromFile "digestMd5sv.txt"
			=$= pipeCl digestMd5Cl
			=$= toStdout
	print r
