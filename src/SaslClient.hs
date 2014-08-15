{-# LANGUAGE TypeFamilies, FlexibleContexts, PackageImports #-}

module SaslClient ( SaslState(..), sasl, convert ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe

import qualified Data.ByteString as BS

import Digest
import XmppCommon

sasl :: (Monad m,
		MonadState m, SaslState (StateType m),
		MonadError m, Error (ErrorType m)
	) => BS.ByteString -> Pipe Common Common m ()
sasl n = saslPipe . fromJust $ find ((== n) . fst) saslClients

saslPipe :: (Monad m, MonadState m, SaslState (StateType m)) => (
		BS.ByteString,
		(Bool, Pipe (Either Success BS.ByteString) BS.ByteString m ())
	) -> Pipe Common Common m ()
saslPipe m =
	inputScramSha1 =$= snd (snd m) =$= outputScramSha1 (fst (snd m)) (fst m)

inputScramSha1 :: Monad m => Pipe Common (Either Success BS.ByteString) m ()
inputScramSha1 = await >>= \mc -> case mc of
	Just (SRChallenge c) -> yield (Right c) >> inputScramSha1
	Just (XCSaslSuccess d) -> yield . Left $ Digest.Success d
	_ -> error "inputScramSha1: bad"

outputScramSha1 :: Monad m =>
	Bool -> BS.ByteString -> Pipe BS.ByteString Common m ()
outputScramSha1 ci mn = do
	if ci
	then await >>= maybe (return ()) (yield . XCAuth mn . Just)
	else yield $ XCAuth mn Nothing
	convert SRResponse

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) ((>> convert f) . yield . f)
