{-# LANGUAGE OverloadedStrings, PackageImports, FlexibleContexts #-}

module NewSasl (
	SaslState(..), Server(..), Client(..), Send, Receive, pipeCl, pipeSv,

	ExampleState(..), fromFile, toStdout ) where

import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Server s m =
	Server (Maybe (Receive s m)) [(Send s m, Receive s m)] (Maybe (Send s m))
data Client s m =
	Client (Maybe (Send s m)) [(Receive s m, Send s m)] (Maybe (Receive s m))

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

pipeSv :: Monad m => Server s m -> Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeSv (Server (Just rcv) srs send') = await >>=
	maybe (return ()) ((>> pipeSv (Server Nothing srs send')) . lift . rcv)
pipeSv (Server _ [] (Just send')) = lift send' >>= yield
pipeSv (Server _ [] _) = return ()
pipeSv (Server _ ((send, rcv) : srs) send') = do
	lift send >>= yield
	await >>= maybe (return ())
		((>> pipeSv (Server Nothing srs send')) . lift . rcv)

class SaslState s where
	getSaslState :: s -> [(BS.ByteString, BS.ByteString)]
	putSaslState :: [(BS.ByteString, BS.ByteString)] -> s -> s

data ExampleState = ExampleState [(BS.ByteString, BS.ByteString)]
	deriving Show

instance SaslState ExampleState where
	getSaslState (ExampleState s) = s
	putSaslState s _ = ExampleState s

pipeCl :: Monad m => Client s m -> Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeCl (Client (Just i) rss rcv') =
	lift i >>= yield >> pipeCl (Client Nothing rss rcv')
pipeCl (Client _ [] (Just rcv)) = await >>= maybe (return ()) (lift . rcv)
pipeCl (Client _ [] _) = return ()
pipeCl (Client _ ((rcv, send) : rss) rcv') = await >>= \mbs -> case mbs of
	Just bs -> lift (rcv bs) >> lift send >>= yield >>
		pipeCl (Client Nothing rss rcv')
	_ -> return ()
