{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

import Debug.Trace

import Control.Monad
import "monads-tf" Control.Monad.State
import System.IO

import Data.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Sasl (Sasl(..), SaslState(..), client2server, server2client)
import TestSasl
import DigestMd5
import Papillon

serverFile, clientFile :: String
serverFile = "digestMd5sv.txt"
clientFile = "digestMd5cl.txt"

client :: (Monad m, SaslState s) => [StateT s m BS.ByteString]
client = [mkResponse, return ""]

server :: (Monad m, SaslState s) => [BS.ByteString -> StateT s m ()]
server = [putReceive, const $ return ()]

mkResponse :: (Monad m, SaslState s) => StateT s m BS.ByteString
mkResponse = do
	st <- gets getSaslState
	let	Just ps = lookup "password" st
		Just rlm = lookup "realm" st
		Just n = lookup "nonce" st
		Just q = lookup "qop" st
		Just c = lookup "charset" st
		Just a = lookup "algorithm" st
		Just un = lookup "username" st
		uri = "xmpp/localhost"
		cn = "00DEADBEEF00"
		nc = "00000001"
	modify $ putSaslState $ [
		("username", un),
		("digest-uri", uri),
		("nc", nc),
		("cnonce", cn) ] ++ st
--		("response", rsp) ] ++ st
	return . fromDigestResponse $ DR {
		drUserName = "yoshikuni",
		drRealm = rlm,
		drPassword = ps,
		drCnonce = "00DEADBEEF00",
		drNonce = n,
		drNc = "00000001",
		drQop = q,
		drDigestUri = "xmpp/localhost",
		drCharset = c }

exampleInit :: [(BS.ByteString, BS.ByteString)]
exampleInit = [
	("password", "password"),
	("username", "yoshikuni")
	]

main_ :: IO ()
main_ = do
	sv <- openFile serverFile ReadMode
	sv1 <- BS.hGetLine sv
	let s1 = server2client digestMd5Sasl $ return sv1
	let s2 = client2server s1 mkResponse
	sv2 <- BS.hGetLine sv
	let s3 = server2client s2 $ return sv2
	let s4 = client2server s3 $ return ""
	case s4 of
		Result m -> m `runStateT` ExampleState exampleInit >>= print
		_ -> error "bad"

main :: IO ()
main = do
	r <- (`runStateT` ExampleState exampleInit) $ do
		runPipe $ fromFile serverFile
			=$= pipeCl digestMd5Sasl client server
			=$= toStdout
	print r
	

pipeCl :: Monad m => Sasl s m -> [StateT s m BS.ByteString]
	-> [BS.ByteString -> StateT s m ()]
	-> Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeCl (Result rslt) _ _ = do
	r <- lift rslt
	case r of
		(True, _) -> yield "success"
		_ -> yield "failure"
pipeCl sasl (send : sends) (rcv : rcvs) = await >>= \mbs -> case mbs of
	Just bs -> do
		let sasl' = server2client sasl $ rcv bs >> return bs
		let sasl'' = client2server sasl' send
		lift $ rcv bs
		s <- lift send
		yield s
		pipeCl sasl'' sends rcvs
	_ -> return ()

putReceive :: (Monad m, SaslState s) => BS.ByteString -> StateT s m ()
putReceive bs = do
	let Just ch = parseAtts bs
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

digestMd5Sasl :: (Monad m, SaslState s) => Sasl s m
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
		Just rs <- parseAtts `liftM` getRs
		let	Just un = lookup "username" rs
			Just uri = lookup "digest-uri" rs
			Just nc = lookup "nc" rs
			Just cn = lookup "cnonce" rs
			Just rsp = lookup "response" rs
		st' <- gets getSaslState
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
