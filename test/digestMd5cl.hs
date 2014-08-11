{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

import "monads-tf" Control.Monad.State

import Data.Pipe

import qualified Data.ByteString as BS

import NewSasl
import DigestMd5
import Papillon

exampleInit :: [(BS.ByteString, BS.ByteString)]
exampleInit = [
	("password", "password"),
	("username", "yoshikuni")
	]

main :: IO ()
main = do
	r <- (`runStateT` ExampleState exampleInit) $ do
		runPipe $ fromFile "digestMd5sv.txt"
			=$= pipeCl client server
			=$= toStdout
	print r


client :: (Monad m, SaslState s) => [Send s m]
client = [mkResponse, return ""]

server :: (Monad m, SaslState s) => [Receive s m]
server = [putReceive, const $ return ()]

mkResponse :: (Monad m, SaslState s) => Send s m
mkResponse = do
	st <- gets getSaslState
	let	Just ps = lookup "password" st
		Just rlm = lookup "realm" st
		Just n = lookup "nonce" st
		Just q = lookup "qop" st
		Just c = lookup "charset" st
		Just un = lookup "username" st
		uri = "xmpp/localhost"
		cn = "00DEADBEEF00"
		nc = "00000001"
	modify $ putSaslState $ [
		("username", un),
		("digest-uri", uri),
		("nc", nc),
		("cnonce", cn) ] ++ st
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

putReceive :: (Monad m, SaslState s) => Receive s m
putReceive bs = do
	let Just ch = parseAtts bs
	st <- gets getSaslState
	let	Just rlm = lookup "realm" ch
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
