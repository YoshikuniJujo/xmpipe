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
		(>> return ()) . forkIO $ do
			hGetLine h >>= putStrLn
			hGetLine h >>= putStrLn
			hClose h
