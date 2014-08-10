{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module TestSasl (
	ExampleState(..), runTestSasl, fromStdin, fromFile, toStdout) where

import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Sasl

data ExampleState = ExampleState [(BS.ByteString, BS.ByteString)]
	deriving Show

instance SaslState ExampleState where
	getSaslState (ExampleState s) = s
	putSaslState s _ = ExampleState s

fromStdin :: MonadIO m => Pipe () BS.ByteString m ()
fromStdin = fromHandle stdin

fromFile :: (MonadBaseControl IO m, MonadIO m) =>
	FilePath -> Pipe () BS.ByteString m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

fromHandle :: MonadIO m => Handle -> Pipe () BS.ByteString m ()
fromHandle h = liftIO (BS.hGetLine h) >>= (>> fromHandle h) . yield

toStdout :: MonadIO m => Pipe BS.ByteString () m ()
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)

runTestSasl :: Monad m => Pipe () () (StateT ExampleState m) () -> m ()
runTestSasl p = do
	ret <- (`evalStateT` ExampleState []) $ runPipe p
	case ret of
		Just _ -> return ()
		_ -> error "error"
