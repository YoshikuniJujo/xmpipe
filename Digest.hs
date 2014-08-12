{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	SASL.SaslState(..),
	digestMd5Cl, digestMd5Sv,
	scramSha1Cl,
	) where

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS

import qualified SaslServer as SASL
import qualified SaslClient as SASL

import qualified SaslScramSha1Client as ScramSha1

digestMd5Cl, digestMd5Sv :: (MonadState m, SASL.SaslState (StateType m)) =>
	Pipe BS.ByteString BS.ByteString m ()
digestMd5Cl = SASL.pipeCl SASL.digestMd5Cl
digestMd5Sv = SASL.pipeSv SASL.digestMd5Sv

scramSha1Cl :: (MonadState m, SASL.SaslState (StateType m)) =>
	Pipe BS.ByteString BS.ByteString m ()
scramSha1Cl = SASL.pipeCl
	. (\(ScramSha1.Client _ sc _) -> ScramSha1.Client Nothing sc Nothing)
	$ ScramSha1.scramSha1Client
