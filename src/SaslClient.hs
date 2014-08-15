{-# LANGUAGE TypeFamilies, FlexibleContexts, PackageImports #-}

module SaslClient ( SaslState(..), sasl, convert ) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe

import qualified Data.ByteString as BS

import Network.Sasl
import qualified Network.Sasl.Plain.Client as Pln
import qualified Network.Sasl.External.Client as Ext
import qualified Network.Sasl.DigestMd5.Client as DM5
import qualified Network.Sasl.ScramSha1.Client as SS1
import XmppCommon

saslClients :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) => [(
	BS.ByteString,
	(Bool, Pipe (Either Success BS.ByteString) BS.ByteString m ()) )]
saslClients = [DM5.sasl, SS1.sasl, Pln.sasl, Ext.sasl]

sasl :: (Monad m,
		MonadState m, SaslState (StateType m),
		MonadError m, Error (ErrorType m)
	) => BS.ByteString -> Pipe Xmpp Xmpp m ()
sasl n = saslPipe . fromJust $ find ((== n) . fst) saslClients

saslPipe :: (Monad m, MonadState m, SaslState (StateType m)) => (
		BS.ByteString,
		(Bool, Pipe (Either Success BS.ByteString) BS.ByteString m ())
	) -> Pipe Xmpp Xmpp m ()
saslPipe m =
	inputScramSha1 =$= snd (snd m) =$= outputScramSha1 (fst (snd m)) (fst m)

inputScramSha1 :: Monad m => Pipe Xmpp (Either Success BS.ByteString) m ()
inputScramSha1 = await >>= \mc -> case mc of
	Just (SRChallenge c) -> yield (Right c) >> inputScramSha1
	Just (XCSaslSuccess d) -> yield . Left $ Network.Sasl.Success d
	_ -> error "inputScramSha1: bad"

outputScramSha1 :: Monad m =>
	Bool -> BS.ByteString -> Pipe BS.ByteString Xmpp m ()
outputScramSha1 ci mn = do
	if ci
	then await >>= maybe (return ()) (yield . XCAuth mn . Just)
	else yield $ XCAuth mn Nothing
	convert SRResponse

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) ((>> convert f) . yield . f)
