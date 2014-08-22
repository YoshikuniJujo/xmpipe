{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module ManyChanPipe (
	fromChan, fromChans, toChan,
	) where

import Control.Applicative
-- import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Pipe
-- import Data.Pipe.ByteString
-- import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
-- import System.IO

-- import qualified Data.ByteString as BS

{-
main :: IO ()
main = do
	c1 <- atomically newTChan
	c2 <- atomically newTChan
	forkIO . forever $ do
		sln <- BS.getLine
		case BS.splitAt 2 sln of
			("1 ", ln) -> atomically $ writeTChan c1 ln
			("2 ", ln) -> atomically $ writeTChan c2 ln
			_ -> return ()
	_ <- runPipe (fromChans [c1, c2] =$= toHandleLn stdout :: Pipe () () IO ())
	return ()
	-}

fromChan :: MonadBaseControl IO m => TChan a -> Pipe () a m ()
fromChan c = lift (liftBase . atomically $ readTChan c) >>= yield >> fromChan c

toChan :: MonadBaseControl IO m => TChan a -> Pipe a () m ()
toChan c = await >>=
	maybe (return ()) ((>> toChan c) . lift . liftBase . atomically . writeTChan c)

fromChans :: MonadBaseControl IO m => [TChan a] -> Pipe () a m ()
fromChans cs = (>> fromChans cs) . (yield =<<) . lift . liftBase . atomically $ do
	mx <- readTChans cs
	case mx of
		Just x -> return x
		_ -> retry

readTChans :: [TChan a] -> STM (Maybe a)
readTChans [] = return Nothing 
readTChans (c : cs) = do
	e <- isEmptyTChan c
	if e then readTChans cs else Just <$> readTChan c
