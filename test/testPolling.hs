import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Posix.IO
import GHC.Event

main_ :: IO ()
main_ = do
	Just em <- getSystemEventManager
	registerTimeout em 1000000 (print 888)
	registerFd em (\k e -> getLine >>= print >> print k >> print e) stdInput evtRead
	threadDelay 2000000
	return ()

main :: IO ()
main = do
	c <- atomically newTChan
	Just em <- getSystemEventManager
	registerTimeout em 1000000 (print 888)
	forkIO . void $ registerFd em
		(\k e -> void $ print k >> print e >> atomically (writeTChan c ()))
		stdInput evtRead
	atomically $ readTChan c
	getLine >>= print
	threadDelay 2000000
