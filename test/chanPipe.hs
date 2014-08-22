{-# LANGUAGE PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Data.Char
import Data.Pipe
import Data.Pipe.IO
import System.IO

fromTChan :: MonadIO m => TChan a -> Pipe () a m ()
fromTChan c = liftIO (atomically $ readTChan c) >>= yield >> fromTChan c

toTChan :: MonadIO m => TChan a -> Pipe a () m ()
toTChan c = await >>= maybe (return ())
	((>> toTChan c) . liftIO . atomically . writeTChan c)

main :: IO ()
main = do
	inc <- atomically newTChan
	otc <- atomically newTChan
	forkIO . void . runPipe $ fromFile "test/sample.txt" =$= toTChan inc
	forkIO . void . runPipe $ fromTChan otc =$= toHandle stdout
	void . runPipe $ fromTChan inc =$= convert toUpper =$= toTChan otc
