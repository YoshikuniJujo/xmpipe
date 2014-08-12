{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module SaslScramSha1Client
	(Client(..), scramSha1Client, doesClientHasInit) where

import "monads-tf" Control.Monad.State

import NewSasl
import ScramSha1

import qualified Data.ByteString.Char8 as BSC

scramSha1Client :: (MonadState m, SaslState (StateType m)) => Client m
scramSha1Client =
	Client (Just clientFirst) [(serverFirst, clientFinal)] (Just serverFinal)

clientFirst :: (MonadState m, SaslState (StateType m)) => Send m
clientFirst = do
	st <- gets getSaslState
	let	Just un = lookup "username" st
		Just nnc = lookup "cnonce" st
	return $ clientFirstMessage un nnc

serverFirst :: (MonadState m, SaslState (StateType m)) => Receive m
serverFirst ch = do
	let Just (nnc, slt, i) = readServerFirstMessage ch
	st <- gets getSaslState
	modify . putSaslState $ [
		("nonce", nnc),
		("salt", slt),
		("i", BSC.pack $ show i) ] ++ st

clientFinal :: (MonadState m, SaslState (StateType m)) => Send m
clientFinal = do
	st <- gets getSaslState
	let	Just un = lookup "username" st
		Just ps = lookup "password" st
		Just slt = lookup "salt" st
		Just i = lookup "i" st
		cb = "n,,"
		Just cnonce = lookup "cnonce" st
		Just nonce = lookup "nonce" st
	return $ clientFinalMessage un ps slt (read $ BSC.unpack i) cb cnonce nonce

serverFinal :: (MonadState m, SaslState (StateType m)) => Receive m
serverFinal ch = do
	let Just v = readServerFinalMessage ch
	st <- gets getSaslState
	let	Just un = lookup "username" st
		Just ps = lookup "password" st
		Just slt = lookup "salt" st
		Just i = lookup "i" st
		cb = "n,,"
		Just cnnc = lookup "cnonce" st
		Just nnc = lookup "nonce" st
		sfm = serverFinalMessage un ps slt (read $ BSC.unpack i) cb cnnc nnc
	let Just v' = readServerFinalMessage sfm
	unless (v == v') $ error "serverFinal: bad"
	modify . putSaslState $ [("verify", v), ("sfm", sfm)] ++ st
