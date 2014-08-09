{-# LANGUAGE OverloadedStrings, PackageImports #-}

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Sasl
import qualified SaslMd5 as MD5

data ExampleState = ExampleState [(BS.ByteString, BS.ByteString)]

instance SaslState ExampleState where
	getSaslState (ExampleState s) = s
	putSaslState s _ = ExampleState s

fromStdin :: MonadIO m => Pipe () BS.ByteString m ()
fromStdin = liftIO BS.getLine >>= yield

toStdout :: MonadIO m => Pipe BS.ByteString () m ()
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)

main :: IO ()
main = do
	ps <- BS.getLine
	ret <- (`evalStateT` ExampleState []) . runPipe $
		fromStdin =$= MD5.sasl ps =$= toStdout
	case ret of
		Just _ -> return ()
		_ -> putStrLn "error"
