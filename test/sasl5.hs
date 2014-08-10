import Data.Pipe
import qualified Data.ByteString as BS

import TestSasl
import qualified SaslMd5 as MD5

main :: IO ()
main = do
	ps <- BS.getLine
	runTestSasl $ fromFile "md5sv.txt" =$= MD5.sasl ps =$= toStdout
