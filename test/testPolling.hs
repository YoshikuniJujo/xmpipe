import GHC.Event
import Control.Concurrent
import System.Posix.IO

main :: IO ()
main = do
	Just em <- getSystemEventManager
	registerTimeout em 1000000 (print 888)
	registerFd em (\k e -> getLine >>= print >> print k >> print e) stdInput evtRead
	threadDelay 2000000
	return ()
