{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Network.XMPiPe.Core.S2S.Client (
	-- * Types and Values
	Mpi(..), Jid(..), Tags(..), tagsNull, tagsType,
	-- * Functions
	starttls, sasl, begin, input, output,
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

import SaslClient hiding (sasl)
import qualified SaslClient as S
import Xmpp hiding (input, output)

input :: Monad m => [Xmlns] -> Pipe BS.ByteString Mpi m ()
input = inputMpi

output :: Monad m => Pipe Mpi BS.ByteString m ()
output = outputMpi

starttls :: Monad m =>
	BS.ByteString -> BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
starttls fr to = inputP3 =$= processTls fr to =$= outputS

processTls :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
processTls fr to = do
	yield XCDecl
	yield $ XCBegin [(From, fr), (To, to), (TagRaw $ nullQ "version", "1.0")]
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
	MonadError m, Error (ErrorType m) ) =>
	BS.ByteString -> BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
sasl fr to = inputP3 =$= processSasl fr to =$= outputS

processSasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) =>
	BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
processSasl fr to = do
	yield XCDecl
	yield $ XCBegin [ (From, fr), (To, to), (TagRaw $ nullQ "version", "1.0")]
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

begin :: Monad m =>
	BS.ByteString -> BS.ByteString -> Pipe BS.ByteString BS.ByteString m [Xmlns]
begin fr to = inputFeature =@= process fr to =$= outputS

process :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
process fr to = do
	yield XCDecl
	yield $ XCBegin [(From, fr), (To, to), (TagRaw $ nullQ "version", "1.0")]
	Just (XCBegin _as) <- await
	Just (XCFeatures []) <- await
	_ <- await
	return ()
