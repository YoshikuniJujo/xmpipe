{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Network.XMPiPe.Core.S2S.Client (
	-- * Types and Values
	Mpi(..), Jid(..), Tags(..), tagsType,
	-- * Functions
	starttls, sasl, begin, input, outputMpi
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe

import qualified Data.ByteString as BS

import Tools
import SaslClient hiding (sasl)
import qualified SaslClient as S
import Xmpp

starttls :: Monad m => Pipe BS.ByteString BS.ByteString m ()
starttls = inputP3 =$= processTls =$= outputS

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = do
	yield XCDecl
	yield $ XCBegin [(From, "localhost"), (To, "otherhost"),
		(TagRaw $ nullQ "version", "1.0")]
	procTls

procTls :: Monad m => Pipe Xmpp Xmpp m ()
procTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> procTls
	Just (XCFeatures [FtStarttls _]) -> do
		yield XCStarttls
		procTls
	Just XCProceed -> return ()
	Just _ -> return ()
	_ -> return ()

sasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m)
	) =>
	Pipe BS.ByteString BS.ByteString m ()
sasl = inputP3 =$= processSasl =$= outputS

processSasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m)
	) => Pipe Xmpp Xmpp m ()
processSasl = do
	yield XCDecl
	yield $ XCBegin [
		(From, "localhost"),
		(To, "otherhost"),
		(TagRaw $ nullQ "version", "1.0")]
	procSasl

procSasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m)
	) => Pipe Xmpp Xmpp m ()
procSasl = await >>= \mx -> case mx of
	Just (XCBegin _as) -> procSasl
	Just (XCFeatures [FtMechanisms ["EXTERNAL"]]) -> do
		st <- lift $ gets getSaslState
		lift . modify . putSaslState $ ("username", "") : st
		S.sasl "EXTERNAL"
		lift . modify $ putSaslState st
	_ -> return ()

begin :: Monad m => Pipe BS.ByteString BS.ByteString m ()
begin = inputP2 =$= process =$= outputS

process :: Monad m => Pipe Xmpp Xmpp m ()
process = do
	yield XCDecl
	yield $ XCBegin [
		(From, "localhost"),
		(To, "otherhost"),
		(TagRaw $ nullQ "version", "1.0")]
	Just (XCBegin _as) <- await
	Just (XCFeatures []) <- await
	return ()
