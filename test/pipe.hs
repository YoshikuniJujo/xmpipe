{-# LANGUAGE PackageImports, FlexibleContexts #-}

import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString.Char8 as BSC

fromFile :: (MonadBaseControl IO m, MonadIO m) =>
	FilePath -> Pipe () BSC.ByteString m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

fromHandle :: MonadIO m => Handle -> Pipe () BSC.ByteString m ()
fromHandle h = liftIO (BSC.hGetLine h) >>= (>> fromHandle h) . yield

toStdout :: MonadIO m => Pipe BSC.ByteString () m ()
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)
