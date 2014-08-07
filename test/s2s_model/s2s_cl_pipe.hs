{-# LANGUAGE PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Maybe
import Data.Pipe
import System.IO
import Network

main :: IO ()
main = run Nothing

run :: Maybe String -> IO ()
run ms = do
	h <- connectTo "localhost" $ PortNumber 54492
	fromJust <$> runPipe (fromStdin =$= mkData =$= output h)

fromStdin :: Pipe () String IO ()
fromStdin = do
	l <- liftIO getLine
	yield l
	fromStdin

mkData :: Monad m => Pipe String String m ()
mkData = await >>= \mx -> case mx of
	Just x -> yield x >> mkData
	_ -> return ()

output :: Handle -> Pipe String () IO ()
output h = await >>= \mx -> case mx of
	Just x -> do
		mr <- liftIO . (`catch` conHandle x) $ do
			hPutStrLn h x
			return Nothing
		case mr of
			Just r -> do
				h' <- liftIO $ do
					hh <- connectTo "localhost" $
						PortNumber 54492
					hPutStrLn hh r
					return hh
				output h'
			_ -> output h
	_ -> return ()

conHandle :: String -> IOException -> IO (Maybe String)
conHandle x e = do
	putStrLn $ "ERROR OCCUR: " ++ show e
	return $ Just x
