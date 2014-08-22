import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
	c1 <- atomically newTChan
	c2 <- atomically newTChan
	_ <- forkIO . forever $ do
		sln <- getLine
		case sln of
			'1' : ' ' : ln -> atomically $ writeTChan c1 ln
			'2' : ' ' : ln -> atomically $ writeTChan c2 ln
			_ -> return ()
	forever $ do
		ln <- atomically $ do
			e1 <- isEmptyTChan c1
			e2 <- isEmptyTChan c2
			case (e1, e2) of
				(False, _) -> readTChan c1
				(_, False) -> readTChan c2
				_ -> retry
		print ln
