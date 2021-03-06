{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module SaslServer (
	SaslState(..), SaslError(..),
--	saslServers,
	SaslErrorType(..), Success(..),
	mkSaslServers, Retrieve(..),

	runSasl,
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Network.Sasl

import qualified Data.ByteString as BS
import qualified Network.Sasl.Plain.Server as Pln
import qualified Network.Sasl.External.Server as Ext
import qualified Network.Sasl.DigestMd5.Server as DM5
import qualified Network.Sasl.ScramSha1.Server as SS1

import XmppType

{-
sampleRetrieves :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m)) => [Retrieve m]
sampleRetrieves = [
	RTPlain retrievePln, RTExternal retrieveEx,
	RTDigestMd5 retrieveDM5, RTScramSha1 retrieveSS1 ]
	-}

runSasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	[Retrieve m] -> Pipe Xmpp Xmpp m ()
runSasl rt = do
	yield $ XCFeatures [FtMechanisms $ map getMechanism rt]
	await >>= \a -> case a of
		Just (XCAuth m i) -> sasl_ rt m i
		_ -> throwError $ fromSaslError
			(SaslErrorType "EOF") "unexpected EOF"

sasl_ :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	[Retrieve m] -> BS.ByteString -> Maybe BS.ByteString -> Pipe Xmpp Xmpp m ()
sasl_ rt n i = case lookup n $ mkSaslServers rt of
	Just (b, s) -> saslPipe b i s
	_ -> throwError $ fromSaslError InvalidMechanism "no such mechanisms"

saslPipe :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => Bool
	-> Maybe BS.ByteString
	-> Pipe BS.ByteString (Either Success BS.ByteString) m ()
	-> Pipe Xmpp Xmpp m ()
saslPipe True (Just i) s =
	(yield i >> convert (\(SRResponse r) -> r)) =$= s =$= saslOutput
saslPipe True _ s = convert (\(SRResponse r) -> r) =$= s
		=$= (yield (SRChallenge "") >> saslOutput)
saslPipe False Nothing s = convert (\(SRResponse r) -> r) =$= s =$= saslOutput
saslPipe _ _ _ = throwError $
	fromSaslError MalformedRequest "no need of initial data"

saslOutput :: (MonadState m, SaslState (StateType m)) =>
	Pipe (Either Success BS.ByteString) Xmpp m ()
saslOutput = await >>= \mch -> case mch of
	Just (Right r) -> yield (SRChallenge r) >> saslOutput
	Just (Left (Success r)) -> yield $ XCSaslSuccess r
	_ -> return ()

data Retrieve m
	= RTPlain (BS.ByteString -> BS.ByteString -> BS.ByteString -> m ())
	| RTExternal (BS.ByteString -> m ())
	| RTDigestMd5 (BS.ByteString -> m BS.ByteString)
	| RTScramSha1 (BS.ByteString ->
		m (BS.ByteString, BS.ByteString, BS.ByteString, Int))

getMechanism :: Retrieve m -> BS.ByteString
getMechanism (RTPlain _) = "PLAIN"
getMechanism (RTExternal _) = "EXTERNAL"
getMechanism (RTDigestMd5 _) = "DIGEST-MD5"
getMechanism (RTScramSha1 _) = "SCRAM-SHA-1"

mkSaslServers :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m)) => [Retrieve m] -> [(
	BS.ByteString,
	(Bool, Pipe BS.ByteString (Either Success BS.ByteString) m ()) )]
mkSaslServers = map $ \rts -> case rts of
	RTPlain rt -> Pln.sasl rt
	RTExternal rt -> Ext.sasl rt
	RTDigestMd5 rt -> DM5.sasl rt
	RTScramSha1 rt -> SS1.sasl rt
