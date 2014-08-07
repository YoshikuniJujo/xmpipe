{-# LANGUAGE PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Data.Maybe
import Data.Pipe
import System.IO
import Network

import Data.Char

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 54492
	vh <- atomically $ newTVar h
	fromJust <$> runPipe (input vh =$= process =$= output vh)

input :: TVar Handle -> Pipe () String IO ()
input vh = do
	h <- liftIO . atomically $ readTVar vh
	maybe (return ()) yield =<< liftIO ((`catch` reconnect vh Nothing)
		. (Just <$>) $ hGetLine h)
	input vh

output :: TVar Handle -> Pipe String () IO ()
output vh = do
	h <- liftIO . atomically $ readTVar vh
	await >>= \mx -> case mx of
		Just x -> do
			liftIO . (`catch` reconnect vh (Just x)) $ do
				hPutStrLn h x
				return Nothing
			output vh
		_ -> return ()

reconnect :: TVar Handle -> Maybe String -> IOException -> IO (Maybe String)
reconnect vh x _ = do
	h <- connectTo "localhost" $ PortNumber 54492
	maybe (return ()) (hPutStrLn h) x
	atomically $ writeTVar vh h
	return Nothing

process :: Monad m => Pipe String String m ()
process = await >>= \mx -> case mx of
	Just x -> yield (map toUpper x) >> process
	_ -> return ()
