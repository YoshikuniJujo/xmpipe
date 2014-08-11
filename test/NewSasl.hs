{-# LANGUAGE OverloadedStrings, PackageImports, FlexibleContexts #-}

module NewSasl (
	SaslState(..), Send, Receive, pipeCl, pipeSv,

	ExampleState(..), fromFile, toStdout,
	) where

import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

type Send s m = StateT s m BS.ByteString
type Receive s m = BS.ByteString -> StateT s m ()

fromFile :: (MonadBaseControl IO m, MonadIO m) =>
	FilePath -> Pipe () BS.ByteString m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

fromHandle :: MonadIO m => Handle -> Pipe () BS.ByteString m ()
fromHandle h = liftIO (BS.hGetLine h) >>= (>> fromHandle h) . yield

toStdout :: MonadIO m => Pipe BS.ByteString () m ()
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)

pipeSv :: Monad m => [StateT s m BS.ByteString]
	-> [BS.ByteString -> StateT s m ()]
	-> Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeSv [] [] = return ()
pipeSv [send] [] = lift send >>= yield
pipeSv (send : sends) (rcv : rcvs) = do
	lift send >>= yield
	await >>= \mbs -> case mbs of
		Just bs -> do
			lift $ rcv bs
			pipeSv sends rcvs
		_ -> return ()
pipeSv _ _ = error "pipeSv: bad"

class SaslState s where
	getSaslState :: s -> [(BS.ByteString, BS.ByteString)]
	putSaslState :: [(BS.ByteString, BS.ByteString)] -> s -> s

data ExampleState = ExampleState [(BS.ByteString, BS.ByteString)]
	deriving Show

instance SaslState ExampleState where
	getSaslState (ExampleState s) = s
	putSaslState s _ = ExampleState s

pipeCl :: Monad m => [StateT s m BS.ByteString]
	-> [BS.ByteString -> StateT s m ()]
	-> Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeCl [] [] = await >>= \mbs -> case mbs of
	Just _bs -> return ()
	_ -> return ()
pipeCl (send : sends) (rcv : rcvs) = await >>= \mbs -> case mbs of
	Just bs -> do
		lift $ rcv bs
		s <- lift send
		yield s
		pipeCl sends rcvs
	_ -> return ()
pipeCl _ _ = error "pipeCl: bad"
