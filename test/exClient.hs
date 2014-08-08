{-# LANGUAGE ScopedTypeVariables #-}

import Data.Typeable

import Control.Exception
import GHC.IO.Exception
import System.IO
import Network

main :: IO ()
main = test `catch` ioHandle

test :: IO ()
test = do
	h <- connectTo "localhost" $ PortNumber 54492
	hGetLine h >>= putStrLn
	(hGetLine h >>= putStrLn) `catch` ioHandle
	hGetLine h >>= putStrLn

ioHandle :: IOException -> IO ()
ioHandle e = do
	print $ ioe_handle e
	print $ ioe_type e
	print $ ioe_location e
	print $ ioe_description e
	print $ ioe_errno e
	print $ ioe_filename e
