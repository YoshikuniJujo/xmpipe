import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Network

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 54492
	forever $ do
		(h, _, _) <- accept soc
		((>> return ()) . forkIO) . replicateM_ 3 $ do
			i <- getLine
			hPutStrLn h i
			putStrLn $ "send: " ++ i
			hGetLine h >>= putStrLn
