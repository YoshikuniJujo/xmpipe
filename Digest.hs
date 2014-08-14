{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	SASL.Success(..), SASL.SaslState(..), saslClients, saslServers ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe

import qualified Data.ByteString as BS

import qualified Network.Sasl as SASL
import qualified Network.Sasl.DigestMd5.Server as DM5S
import qualified Network.Sasl.DigestMd5.Client as DM5C

import qualified Network.Sasl.ScramSha1.Client as SS1C
import qualified Network.Sasl.ScramSha1.Server as SS1S

saslClients :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) => [(
	BS.ByteString,
	(Bool, Pipe (Either SASL.Success BS.ByteString) BS.ByteString m ()) )]
saslClients = [DM5C.sasl, SS1C.sasl]

saslServers :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m)) => [(
	BS.ByteString,
	(Bool, Pipe BS.ByteString (Either SASL.Success BS.ByteString) m ()) )]
saslServers = [("DIGEST-MD5", digestMd5Sv), ("SCRAM-SHA-1", scramSha1Sv)]

digestMd5Sv :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m)) =>
	(Bool, Pipe BS.ByteString (Either SASL.Success BS.ByteString) m ())
digestMd5Sv = SASL.server . DM5S.digestMd5Sv $
	\"yoshikuni" -> DM5S.mkStored "yoshikuni" "localhost" "password"

scramSha1Sv :: (
		MonadState m, SASL.SaslState (StateType m),
		MonadError m, Error (ErrorType m) ) =>
	(Bool, Pipe BS.ByteString (Either SASL.Success BS.ByteString) m ())
scramSha1Sv = SASL.server . SS1S.scramSha1Server $
	\"yoshikuni" -> (slt, stk, svk, i)
	where
	slt = "pepper"
	i = 4492
	(stk, svk) = SS1S.salt "password" slt i
