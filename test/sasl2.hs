{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5

data Sasl a b
	= C2S ((a -> (BS.ByteString, b)) -> Sasl a b)
	| S2C ((b -> (BS.ByteString, a)) -> Sasl a b)
	| Success (Maybe BS.ByteString)
	| Failure (Maybe BS.ByteString)

exampleSasl :: BS.ByteString -> Sasl BS.ByteString BS.ByteString
exampleSasl ps = S2C $ \ch -> C2S $ \rs ->
	if rs (ch ps) == MD5.hash (ch ps `BS.append` ps)
		then Success Nothing
		else Failure Nothing

client2server :: Sasl a b -> (a -> BS.ByteString) -> Sasl a b
client2server (C2S f) bs = f bs
client2server s@(S2C _) _ = s
client2server _ _ = error "bad"

server2client :: Sasl a b -> (b -> BS.ByteString) -> Sasl a b
server2client (S2C f) bs = f bs
server2client s@(C2S _) _ = s
server2client _ _ = error "bad"

main_ :: IO ()
main_ = do
	let	s0 = exampleSasl "password"
		s1 = server2client s0 $ const "challenge"
	inp <- BS.getLine
	case client2server s1 (\ch -> MD5.hash $ ch `BS.append` inp) of
		Success _ -> putStrLn "succeed"
		Failure _ -> putStrLn "failure"

main :: IO ()
main = do
	let s0 = exampleSasl "password"
	ch <- BS.getLine
	let	s1 = server2client s0 $ const ch
		s2 = client2server s1 $ \ch -> MD5.hash $ ch `BS.append` "password"
	case s2 of
		Success _ -> putStrLn "succeed"
		Failure _ -> putStrLn "failure"
