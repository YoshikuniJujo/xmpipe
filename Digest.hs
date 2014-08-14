{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Digest (
	SASL.Success(..), SASL.SaslState(..), saslClients, saslServers ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Network.Sasl as SASL
import qualified Network.Sasl.DigestMd5.Client as DM5C
import qualified Network.Sasl.DigestMd5.Server as DM5S
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
saslServers = [DM5S.sasl retrieveDM5, SS1S.sasl retrieveSS1]

retrieveDM5 :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) => BS.ByteString -> m BS.ByteString
retrieveDM5 "yoshikuni" = return $ DM5S.mkStored "yoshikuni" "localhost" "password"
retrieveDM5 _ = error "retrieveDM5: no such user"

retrieveSS1 :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) =>
	BS.ByteString -> m (BS.ByteString, BS.ByteString, BS.ByteString, Int)
retrieveSS1 "yoshikuni" = return (slt, stk, svk, i)
	where slt = "pepper"; i = 4492; (stk, svk) = SS1S.salt "password" slt i
retrieveSS1 _ = error "no such user"
