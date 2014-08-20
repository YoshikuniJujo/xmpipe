import Data.Pipe
import Data.Pipe.IO
import System.IO

some :: Pipe () Char IO Int
some = fromFile "test/sample.txt" >> return 888

passResult :: Monad m => Pipe i a m r -> Pipe i (Either a r) m ()
passResult s = do
	r <- mapOut Left s
	yield $ Right r

recvResult :: Monad m => Pipe a o m () -> Pipe (Either a r) o m r
recvResult p =
	(p >> return undefined) |||| (await >>= maybe (return undefined) return)

-- withResult :: Pipe () () IO Int
-- s =@= p = passResult s =$= recvResult p

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	await >>= maybe (return ()) yield
	takeP (n - 1)
