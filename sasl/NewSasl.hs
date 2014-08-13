{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, PackageImports #-}

module NewSasl (
	SaslState(..), Send, Receive, Result(..),
	Client(..), pipeCl, doesClientHasInit,
	Server(..), pipeSv,

	ExampleState(..), fromFile, toStdout ) where

import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Server m = Server (Maybe (Receive m)) [(Send m, Receive m)] (Maybe (Send m))
data Client m = Client (Maybe (Send m)) [(Receive m, Send m)] (Maybe (Receive m))

type Send m = m BS.ByteString
type Receive m = BS.ByteString -> m ()

data Result = Result Bool (Maybe BS.ByteString)

doesClientHasInit :: Client m -> Bool
doesClientHasInit (Client (Just _) _ _) = True
doesClientHasInit _ = False

fromFile :: (MonadBaseControl IO m, MonadIO m) =>
	FilePath -> Pipe () BS.ByteString m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

fromHandle :: MonadIO m => Handle -> Pipe () BS.ByteString m ()
fromHandle h = liftIO (BS.hGetLine h) >>= (>> fromHandle h) . yield

toStdout :: MonadIO m => Pipe BS.ByteString () m ()
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)

pipeSv :: Monad m =>
	Server m -> Pipe BS.ByteString (Either Result BS.ByteString) m ()
pipeSv (Server (Just rcv) srs send') = await >>=
	maybe (return ()) ((>> pipeSv (Server Nothing srs send')) . lift . rcv)
pipeSv (Server _ [] (Just send')) = lift send' >>= yield . Left . Result True . Just
pipeSv (Server _ [] _) = return ()
pipeSv (Server _ ((send, rcv) : srs) send') = do
	lift send >>= yield . Right
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

-- pipeCl :: Monad m => Client m -> Pipe BS.ByteString BS.ByteString m ()
pipeCl :: Monad m =>
 	Client m -> Pipe (Either Result BS.ByteString) BS.ByteString m ()
pipeCl (Client (Just i) rss rcv') =
	lift i >>= yield >> pipeCl (Client Nothing rss rcv')
pipeCl (Client _ [] (Just rcv)) = await >>= \mi -> case mi of
	Just (Left (Result True (Just d))) -> lift $ rcv d
	Just (Right d) -> lift (rcv d) >> yield "" >> await >>= \mi' -> case mi' of
		Just (Left (Result True Nothing)) -> return ()
		_ -> error "pipeCl: bad"
	_ -> return ()
-- maybe (return ()) (lift . rcv)
pipeCl (Client _ [] _) = await >> return ()
pipeCl (Client _ ((rcv, send) : rss) rcv') = await >>= \mbs -> case mbs of
	Just (Right bs) -> lift (rcv bs) >> lift send >>= yield >>
		pipeCl (Client Nothing rss rcv')
	_ -> return ()
