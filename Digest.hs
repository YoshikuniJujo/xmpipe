{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	SASL.Result(..),
	SASL.SaslState(..),
	digestMd5Cl, digestMd5Sv,

	scramSha1Cl, scramSha1Sv,
--	doesClientHasInit,
	ScramSha1.doesServerWantInit,
	) where

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS

import qualified SaslServer as SASL
import qualified SaslClient as SASL

import qualified SaslScramSha1Client as ScramSha1
import qualified SaslScramSha1Server as ScramSha1

digestMd5Cl :: (MonadState m, SASL.SaslState (StateType m)) => (
	BS.ByteString,
	(Pipe (Either SASL.Result BS.ByteString) BS.ByteString m (), Bool))
digestMd5Cl = ("DIGEST-MD5", (SASL.pipeCl SASL.digestMd5Cl, False))

scramSha1Cl :: (MonadState m, SASL.SaslState (StateType m)) => (
	BS.ByteString,
	(Pipe (Either SASL.Result BS.ByteString) BS.ByteString m (), Bool))
scramSha1Cl = ("SCRAM-SHA-1", (SASL.pipeCl ScramSha1.scramSha1Client, True))

digestMd5Sv :: (MonadState m, SASL.SaslState (StateType m)) =>
	Pipe BS.ByteString (Either SASL.Result BS.ByteString) m ()
digestMd5Sv = SASL.pipeSv SASL.digestMd5Sv

scramSha1Sv :: (MonadState m, SASL.SaslState (StateType m)) =>
	Pipe BS.ByteString (Either SASL.Result BS.ByteString) m ()
scramSha1Sv = SASL.pipeSv ScramSha1.scramSha1Server
