{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module S2sClient (
	Xmpp(..), Feature(..), Tag(..),

	SaslState(..),
	toHandleLike, fromHandleLike, outputS, inputP3, hlpDebug,
	SHandle(..),

	starttls, sasl,
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
	MonadError m, Error (ErrorType m),
	MonadIO m ) =>
	Pipe BS.ByteString BS.ByteString m ()
sasl = inputP3 =$= processSasl =$= outputS

processSasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m ) => Pipe Xmpp Xmpp m ()
processSasl = do
	yield XCDecl
	yield $ XCBegin [
		(From, "localhost"),
		(To, "otherhost"),
		(TagRaw $ nullQ "version", "1.0")]
	procSasl

procSasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m),
	MonadIO m) => Pipe Xmpp Xmpp m ()
procSasl = await >>= \mx -> case mx of
	Just (XCBegin _as) -> procSasl
	Just (XCFeatures [FtMechanisms ["EXTERNAL"]]) -> do
		st <- lift $ gets getSaslState
		lift . modify . putSaslState $ ("username", "") : st
		S.sasl "EXTERNAL"
		lift . modify $ putSaslState st
	_ -> return ()
