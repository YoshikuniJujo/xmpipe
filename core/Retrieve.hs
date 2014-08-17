{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Retrieve (retrievePln, retrieveEx, retrieveDM5, retrieveSS1) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error

import qualified Data.ByteString as BS
import qualified Network.Sasl as SASL
import qualified Network.Sasl.DigestMd5.Server as DM5S
import qualified Network.Sasl.ScramSha1.Server as SS1S

retrievePln :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, SASL.SaslError (ErrorType m) ) =>
	BS.ByteString -> BS.ByteString -> BS.ByteString -> m ()
retrievePln "" "yoshikuni" "password" = return ()
retrievePln _ _ _ = throwError $
	SASL.fromSaslError SASL.NotAuthorized "incorrect username or password"

retrieveEx :: (MonadError m, SASL.SaslError (ErrorType m)) => BS.ByteString -> m ()
retrieveEx "" = return ()
retrieveEx hn = throwError $ SASL.fromSaslError SASL.NotAuthorized hn

retrieveDM5 :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, SASL.SaslError (ErrorType m) ) =>
	BS.ByteString -> m BS.ByteString
retrieveDM5 "yoshikuni" = return $ DM5S.mkStored "yoshikuni" "localhost" "password"
retrieveDM5 _ = throwError $
	SASL.fromSaslError SASL.NotAuthorized "incorrect username or password"

retrieveSS1 :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, SASL.SaslError (ErrorType m) ) =>
	BS.ByteString -> m (BS.ByteString, BS.ByteString, BS.ByteString, Int)
retrieveSS1 "yoshikuni" = return (slt, stk, svk, i)
	where slt = "pepper"; i = 4492; (stk, svk) = SS1S.salt "password" slt i
retrieveSS1 _ = throwError $
	SASL.fromSaslError SASL.NotAuthorized "incorrect username or password"
