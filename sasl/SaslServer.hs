{-# LANGUAGE OverloadedStrings, PackageImports, FlexibleContexts #-}

module SaslServer (
	pipeSv, digestMd5Sv,

	ExampleState(..), fromFile, toStdout,
	) where

import "monads-tf" Control.Monad.State

import qualified Data.ByteString as BS

import DigestMd5
import Papillon
import NewSasl

digestMd5Sv :: (MonadState m, SaslState (StateType m)) => Server m
digestMd5Sv = Server Nothing (zip server client) Nothing

server :: (MonadState m, SaslState (StateType m)) => [Send m]
server = [mkChallenge, mkRspAuth, mkResult]

client :: (MonadState m, SaslState (StateType m)) => [Receive m]
client = [putResponse, \"" -> return ()]

mkChallenge, mkRspAuth, mkResult ::
	(MonadState m, SaslState (StateType m)) => Send m
mkChallenge = do
	st <- gets getSaslState
	let	Just rlm = lookup "realm" st
		Just n = lookup "nonce" st
		Just q = lookup "qop" st
		Just c = lookup "charset" st
		Just a = lookup "algorithm" st
	return $ fromDigestMd5Challenge $ DigestMd5Challenge {
		realm = rlm,
		nonce = n,
		qop = q,
		charset = c,
		algorithm = a }

mkRspAuth = do
	st <- gets getSaslState
	let	Just un = lookup "username" st
		Just rlm = lookup "realm" st
		Just ps = lookup "password" st
		Just q = lookup "qop" st
		Just uri = lookup "digest-uri" st
		Just n = lookup "nonce" st
		Just nc = lookup "nc" st
		Just cn = lookup "cnonce" st
		Just rsp = lookup "response" st
		clc = digestMd5 True un rlm ps q uri n nc cn
		cls = digestMd5 False un rlm ps q uri n nc cn
	unless (clc == rsp) $ error "mkRspAuth: bad"
	return $ "rspauth=" `BS.append` cls

mkResult = return "success"

putResponse :: (MonadState m, SaslState (StateType m)) => Receive m
putResponse bs = do
	st <- gets getSaslState
	let	Just rs = parseAtts bs
		Just rlm = lookup "realm" rs
		Just n = lookup "nonce" rs
		Just q = lookup "qop" rs
		Just c = lookup "charset" rs
		Just un = lookup "username" rs
		Just uri = lookup "digest-uri" rs
		Just cn = lookup "cnonce" rs
		Just nc = lookup "nc" rs
		Just rsp = lookup "response" rs
	modify . putSaslState $ [
		("realm", rlm),
		("nonce", n),
		("qop", q),
		("charset", c),
		("username", un),
		("digest-uri", uri),
		("cnonce", cn),
		("nc", nc),
		("response", rsp) ] ++ st
