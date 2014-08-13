{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module SaslScramSha1Server (scramSha1Server, doesServerWantInit) where

import "monads-tf" Control.Monad.State

import qualified Data.ByteString.Char8 as BSC

import NewSasl
import ScramSha1


scramSha1Server :: (MonadState m, SaslState (StateType m)) => Server m
scramSha1Server =
	Server (Just clientFirst) [(serverFirst, clientFinal)] (Just serverFinal)

clientFirst :: (MonadState m, SaslState (StateType m)) => Receive m
clientFirst rs = do
	let Just (un, cnnc) = readClientFirstMessage rs
	st <- gets getSaslState
	modify . putSaslState $ [
		("username", un),
		("cnonce", cnnc) ] ++ st

serverFirst :: (MonadState m, SaslState (StateType m)) => Send m
serverFirst = do
	st <- gets getSaslState
	let	Just cnnc = lookup "cnonce" st
		Just snnc = lookup "snonce" st
		Just slt = lookup "salt" st
		Just i = lookup "i" st
	return $ serverFirstMessage
		(cnnc `BSC.append` snnc) slt (read $ BSC.unpack i)

clientFinal :: (MonadState m, SaslState (StateType m)) => Receive m
clientFinal rs = do
	let Just ("n,,", nnc, prf) = readClientFinalMessage rs
	st <- gets getSaslState
	modify . putSaslState $ [
		("nonce", nnc),
		("proof", prf) ] ++ st

serverFinal :: (MonadState m, SaslState (StateType m)) => Send m
serverFinal = do
	st <- gets getSaslState
	let	Just un = lookup "username" st
		Just ps = lookup "password" st
		Just slt = lookup "salt" st
		Just i = lookup "i" st
		cb = "n,,"
		Just cnnc = lookup "cnonce" st
		Just nnc = lookup "nonce" st
	return $ serverFinalMessage un ps slt (read $ BSC.unpack i) cb cnnc nnc
