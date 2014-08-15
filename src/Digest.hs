{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	Success(..), SaslState(..), DM5.SaslError(..), SaslErrorType(..),
	saslServers ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Network.Sasl

import qualified Data.ByteString as BS
import qualified Network.Sasl.Plain.Server as Pln
import qualified Network.Sasl.External.Server as Ext
import qualified Network.Sasl.DigestMd5.Server as DM5
import qualified Network.Sasl.ScramSha1.Server as SS1

import Retrieve

data Retrieve m
	= RTPlain (BS.ByteString -> BS.ByteString -> BS.ByteString -> m ())
	| RTExternal (BS.ByteString -> m ())
	| RTDigestMd5 (BS.ByteString -> m BS.ByteString)
	| RTScramSha1 (BS.ByteString ->
		m (BS.ByteString, BS.ByteString, BS.ByteString, Int))

saslServers :: (
	MonadState m, SaslState (StateType m),
	MonadError m, DM5.SaslError (ErrorType m)) => [(
	BS.ByteString,
	(Bool, Pipe BS.ByteString (Either Success BS.ByteString) m ()) )]
saslServers = mkSaslServers [
	RTPlain retrievePln, RTExternal retrieveEx,
	RTDigestMd5 retrieveDM5, RTScramSha1 retrieveSS1 ]

mkSaslServers :: (
	MonadState m, SaslState (StateType m),
	MonadError m, DM5.SaslError (ErrorType m)) => [Retrieve m] -> [(
	BS.ByteString,
	(Bool, Pipe BS.ByteString (Either Success BS.ByteString) m ()) )]
mkSaslServers = map $ \rts -> case rts of
	RTPlain rt -> Pln.sasl rt
	RTExternal rt -> Ext.sasl rt
	RTDigestMd5 rt -> DM5.sasl rt
	RTScramSha1 rt -> SS1.sasl rt
