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
	ret <- fromJust <$> runPipe (fromStdin =$= mkData ms =$= output h)
	case ret of
		Just rest -> run $ Just rest
		_ -> return ()

fromStdin :: Pipe () String IO ()
fromStdin = do
	l <- liftIO getLine
	yield l
	fromStdin

mkData :: Monad m => Maybe String -> Pipe String String m ()
mkData mr = do
	maybe (return ()) yield mr
	await >>= \mx -> case mx of
		Just x -> yield x >> mkData Nothing
		_ -> return ()

output :: Handle -> Pipe String () IO (Maybe String)
output h = await >>= \mx -> case mx of
	Just x -> do
		mr <- liftIO . (`catch` conHandle x) $ do
			hPutStrLn h x
			return Nothing
		case mr of
			Just _ -> return mr
			_ -> output h
	_ -> return Nothing

conHandle :: String -> IOException -> IO (Maybe String)
conHandle x e = do
	putStrLn $ "ERROR OCCUR: " ++ show e
	return $ Just x
