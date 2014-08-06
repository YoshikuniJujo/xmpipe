import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

main :: IO ()
main = do
	c <- threadA
	replicateM_ 4 . atomically $ writeTChan c 10

threadA :: IO (TChan Int)
threadA = do
	c <- atomically newTChan
	forkIO . forever $ do
		x <- atomically $ readTChan c
		print x
	return c
