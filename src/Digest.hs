{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	SASL.Success(..), SASL.SaslState(..), DM5S.SaslError(..),
	SASL.SaslErrorType(..),
	saslClients, saslServers ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Network.Sasl as SASL
import qualified Network.Sasl.Plain.Client as PlnC
import qualified Network.Sasl.Plain.Server as PlnS
import qualified Network.Sasl.External.Client as ExtC
import qualified Network.Sasl.External.Server as ExtS
import qualified Network.Sasl.DigestMd5.Client as DM5C
import qualified Network.Sasl.DigestMd5.Server as DM5S
import qualified Network.Sasl.ScramSha1.Client as SS1C
import qualified Network.Sasl.ScramSha1.Server as SS1S

import Retrieve

saslClients :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) => [(
	BS.ByteString,
	(Bool, Pipe (Either SASL.Success BS.ByteString) BS.ByteString m ()) )]
saslClients = [DM5C.sasl, SS1C.sasl, PlnC.sasl, ExtC.sasl]

data Retrieve m
	= RTPlain (BS.ByteString -> BS.ByteString -> BS.ByteString -> m ())
	| RTExternal (BS.ByteString -> m ())
	| RTDigestMd5 (BS.ByteString -> m BS.ByteString)
	| RTScramSha1 (BS.ByteString ->
		m (BS.ByteString, BS.ByteString, BS.ByteString, Int))

saslServers :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, DM5S.SaslError (ErrorType m)) => [(
	BS.ByteString,
	(Bool, Pipe BS.ByteString (Either SASL.Success BS.ByteString) m ()) )]
saslServers = mkSaslServers [
	RTPlain retrievePln, RTExternal retrieveEx,
	RTDigestMd5 retrieveDM5, RTScramSha1 retrieveSS1 ]

mkSaslServers :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, DM5S.SaslError (ErrorType m)) => [Retrieve m] -> [(
	BS.ByteString,
	(Bool, Pipe BS.ByteString (Either SASL.Success BS.ByteString) m ()) )]
mkSaslServers = map $ \rts -> case rts of
	RTPlain rt -> PlnS.sasl rt
	RTExternal rt -> ExtS.sasl rt
	RTDigestMd5 rt -> DM5S.sasl rt
	RTScramSha1 rt -> SS1S.sasl rt
