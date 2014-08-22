import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Network

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 4492
	(h1, _, _) <- accept soc
	(h2, _, _) <- accept soc
	c <- atomically newTChan
	forkIO . forever $ hGetLine h1 >>= atomically . writeTChan c
	forkIO . forever $ hGetLine h2 >>= atomically . writeTChan c
	forever $ atomically (readTChan c) >>= print
