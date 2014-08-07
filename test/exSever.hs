import Control.Concurrent
import System.IO
import Network

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 54492
	(h, _, _) <- accept soc
	hPutStrLn h "Good-bye!"
	threadDelay 1000000
	hClose h
