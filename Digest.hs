{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	SASL.Success(..), SASL.SaslState(..), saslClients, saslServers ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe

import qualified Data.ByteString as BS

import qualified Network.Sasl.DigestMd5.Server as SASL
import qualified Network.Sasl.DigestMd5.Client as SASL

import qualified Network.Sasl.ScramSha1.Client as ScramSha1
import qualified Network.Sasl.ScramSha1.Server as ScramSha1

saslClients :: (MonadState m, SASL.SaslState (StateType m), MonadError m) => [(
	BS.ByteString,
	(Pipe (Either SASL.Success BS.ByteString) BS.ByteString m (), Bool) )]
saslClients = [digestMd5Cl, scramSha1Cl]

digestMd5Cl :: (MonadState m, SASL.SaslState (StateType m), MonadError m) => (
	BS.ByteString,
	(Pipe (Either SASL.Success BS.ByteString) BS.ByteString m (), Bool))
digestMd5Cl = make "DIGEST-MD5" SASL.digestMd5Cl

scramSha1Cl :: (MonadState m, SASL.SaslState (StateType m), MonadError m) => (
	BS.ByteString,
	(Pipe (Either SASL.Success BS.ByteString) BS.ByteString m (), Bool))
scramSha1Cl = make "SCRAM-SHA-1" ScramSha1.scramSha1Client

make :: (MonadState m, SASL.SaslState (StateType m), MonadError m) =>
	BS.ByteString -> ScramSha1.Client m -> (
		BS.ByteString,
		(Pipe (Either SASL.Success BS.ByteString) BS.ByteString m (), Bool))
make n s = (n, (\(x, y) -> (y, x)) $ SASL.client s)

saslServers :: (MonadState m, SASL.SaslState (StateType m)) => [(
	BS.ByteString,
	(Pipe BS.ByteString (Either SASL.Success BS.ByteString) m (), Bool) )]
saslServers = [("DIGEST-MD5", digestMd5Sv), ("SCRAM-SHA-1", scramSha1Sv)]

digestMd5Sv :: (MonadState m, SASL.SaslState (StateType m)) =>
	(Pipe BS.ByteString (Either SASL.Success BS.ByteString) m (), Bool)
digestMd5Sv = (\(x, y) -> (y, x)) $ SASL.server SASL.digestMd5Sv

scramSha1Sv :: (MonadState m, SASL.SaslState (StateType m)) =>
	(Pipe BS.ByteString (Either SASL.Success BS.ByteString) m (), Bool)
scramSha1Sv = (\(x, y) -> (y, x)) $ SASL.server ScramSha1.scramSha1Server
