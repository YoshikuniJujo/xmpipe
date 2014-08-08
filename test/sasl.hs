{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Data.Maybe

import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5

type MechanismList = [(BS.ByteString, BS.ByteString -> Sasl)]

type Mechanism a b = (Sasl, MakeChallenge a, MakeResponse b)

type MakeResponse a = a -> [[BS.ByteString] -> BS.ByteString]

type MakeChallenge a = a -> [[BS.ByteString] -> BS.ByteString]

data Sasl
	= C2S (BS.ByteString -> Sasl)
	| S2C (BS.ByteString -> Sasl)
	| Success (Maybe BS.ByteString)
	| Failure (Maybe BS.ByteString)

exampleSasl :: BS.ByteString -> Sasl
exampleSasl ps = S2C $ \ch -> C2S $ \rs -> if rs == MD5.hash (ch `BS.append` ps)
	then Success Nothing
	else Failure Nothing

server2client :: Sasl -> BS.ByteString -> Sasl
server2client (S2C f) bs = f bs
server2client s@(C2S _) "" = s
server2client _ _ = error "bad"

client2server :: Sasl -> BS.ByteString -> Sasl
client2server (C2S f) bs = f bs
client2server s@(S2C _) "" = s
client2server _ _ = error "bad"

exampleMechanisms :: MechanismList
exampleMechanisms = [("EXAMPLE", exampleSasl)]

{-
exampleMechanism :: Mechanism
exampleMechanism = (exampleSasl, mkExampleCl, mkExampleSv)

mkExampleCl :: BS.ByteString -> [BS.ByteString]
mkExampleCl
-}

auth :: MechanismList -> BS.ByteString -> (BS.ByteString -> Sasl)
auth ml m = fromJust $ lookup m ml

main_ :: IO ()
main_ = do
	m <- BS.getLine
	let	s0 = auth exampleMechanisms m "password"
		s1 = server2client s0 "challenge"
	inp <- BS.getLine
	case client2server s1 (MD5.hash $ "challenge" `BS.append` inp) of
		Success _ -> putStrLn "succeed"
		Failure _ -> putStrLn "failure"

main :: IO ()
main = do
	ps <- BS.getLine
	let s0 = auth exampleMechanisms "EXAMPLE" ps
	ch <- BS.getLine
	let	s1 = server2client s0 ch
		s2 = client2server s1 (MD5.hash $ ch `BS.append` "password")
	case s2 of
		Success _ -> putStrLn "succeed"
		Failure _ -> putStrLn "failure"
