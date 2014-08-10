{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

import Debug.Trace

import Control.Monad
import "monads-tf" Control.Monad.State
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Sasl
import TestSasl
import DigestMd5
import Papillon

serverFile, clientFile :: String
serverFile = "digestMd5sv.txt"
clientFile = "digestMd5cl.txt"

main :: IO ()
main = do
	sv <- openFile serverFile ReadMode
	cl <- openFile clientFile ReadMode
	sv1 <- BS.hGetLine sv
	let s1 = server2client digestMd5Sasl $ return sv1
	cl1 <- BS.hGetLine cl
	let s2 = client2server s1 $ return cl1
	sv2 <- BS.hGetLine sv
	let s3 = server2client s2 $ return sv2
	cl2 <- BS.hGetLine cl
	let s4 = client2server s3 $ return cl2
	case s4 of
		Result m -> m `runStateT`
			ExampleState [("password", "password")] >>= print
		_ -> error "bad"

digestMd5Sasl :: (Monad m, SaslState s) => Sasl s m
-- digestMd5Sasl :: Monad m => Sasl s m
digestMd5Sasl =
	S2C $ \getCh -> C2S $ \getRs -> S2C $ \getRA -> C2S $ \getNl -> Result $ do
		Just ch <- parseAtts `liftM` getCh
		st <- gets getSaslState
		let	Just ps = lookup "password" st
			Just rlm = lookup "realm" ch
			Just n = lookup "nonce" ch
			Just q = lookup "qop" ch
			Just c = lookup "charset" ch
			Just a = lookup "algorithm" ch
		modify $ putSaslState $ [
			("realm", rlm),
			("nonce", n),
			("qop", q),
			("charset", c),
			("algorithm", a) ] ++ st
		Just rs <- parseAtts `liftM` getRs
		let	Just un = lookup "username" rs
			Just uri = lookup "digest-uri" rs
			Just nc = lookup "nc" rs
			Just cn = lookup "cnonce" rs
			Just rsp = lookup "response" rs
		st' <- gets getSaslState
		modify $ putSaslState $ [
			("username", un),
			("digest-uri", uri),
			("nc", nc),
			("cnonce", cn),
			("response", rsp) ] ++ st'
		Just ra <- parseAtts `liftM` getRA
		"" <- getNl
		let	Just rspa = lookup "rspauth" ra

			clc = digestMd5 True un rlm ps q uri n nc cn
			cls = digestMd5 False un rlm ps q uri n nc cn
		trace (show ch ++ "\n" ++ show rs ++ "\n" ++ show ra ++ "\n")
			$ return ()
		trace (BSC.unpack clc) $ return ()
		trace (BSC.unpack cls) $ return ()
		return (rsp == clc && rspa == cls, Nothing)
