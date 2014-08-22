{-# LANGUAGE OverloadedStrings, TypeFamilies,
	FlexibleContexts, PackageImports #-}

module XmppClient (
	Mpi(..), Jid(..), toJid,
	Tags(..), tagsNull, tagsChat, tagsGet, tagsResult,
	starttls, sasl, saslInit, bind, input, output,
	) where

import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Writer
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

import Xmpp hiding (input, output)
import qualified Xmpp
import Im
import qualified SaslClient as SASL

input :: Monad m => [Xmlns] -> Pipe BS.ByteString Mpi m ()
input = inputMpi

output :: Monad m => Pipe Mpi BS.ByteString m ()
output = outputMpi

begin :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
begin h l = do
	yield XCDecl
	yield $ XCBegin [(To, h), (TagRaw $ nullQ "version", "1.0"), (Lang, l)]

starttls :: Monad m => BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
starttls hst = inputP3 =$= (begin hst "en" >> starttls_) =$= Xmpp.output

starttls_ :: Monad m => Pipe Xmpp Xmpp m ()
starttls_ = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	unless (any isSt fs) $ fail "starttls_: not support tls"
	yield XCStarttls
	Just XCProceed <- await
	return ()
	where isSt (FtStarttls _) = True; isSt _ = False

sasl :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m)) =>
	BS.ByteString -> [BS.ByteString] -> Pipe BS.ByteString BS.ByteString m ()
sasl hst ms = inputP2 =$= sasl' hst ms =$= Xmpp.output

sasl' :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m)) =>
	BS.ByteString -> [BS.ByteString] -> Pipe Xmpp Xmpp m ()
sasl' hst ms = begin hst "en" >> sasl_ ms

sasl_ :: (Monad m, MonadState m, SASL.SaslState (StateType m), MonadError m, Error (ErrorType m) ) =>
	[BS.ByteString] -> Pipe Xmpp Xmpp m ()
sasl_ sl = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	let	Just (FtMechanisms ms) = find isFtMechanisms fs
		Just n = listToMaybe $ sl `intersect` ms
	SASL.sasl n
	where
	isFtMechanisms (FtMechanisms _) = True
	isFtMechanisms _ = False

bind :: (Monad m,
	MonadWriter m, [FeatureR] ~ WriterType m,
	MonadError m, Error (ErrorType m) ) =>
	BS.ByteString -> Pipe BS.ByteString BS.ByteString m [Xmlns]
bind hst = inputP3 =@= (begin hst "en" >> bind_) =$= Xmpp.output

bind_ :: (
	MonadWriter m, [FeatureR] ~ (WriterType m),
	MonadError m, Error (ErrorType m) ) => Pipe Xmpp Xmpp m ()
bind_ = await >>= \mr -> case mr of
	Just (XCFeatures fs) -> do
		let (b, fs') = sepBind $ map featureToFeatureR fs
		mapM_ yield . catMaybes $ map responseToFeature b
		tell fs'
		bind_
	Just _ -> bind_
	_ -> return ()

sepBind :: [FeatureR] -> ([FeatureR], [FeatureR])
sepBind = partition isFtBind

isFtBind :: FeatureR -> Bool
isFtBind (Ft (FtBind _)) = True
isFtBind _ = False

responseToFeature :: FeatureR -> Maybe Xmpp
responseToFeature (Ft (FtBind _)) = Just
	. SRIqBind [(Type, "set"), (Id, "_xmpp_bind1")] . IqBind Nothing
	$ Resource "profanity"
responseToFeature _ = Nothing

tagsGet :: Tags
tagsGet = Tags Nothing (Just "get") Nothing Nothing Nothing []

tagsChat :: Tags
tagsChat = Tags Nothing (Just "chat") Nothing Nothing Nothing []

tagsResult :: Tags
tagsResult = Tags Nothing (Just "result") Nothing Nothing Nothing []

saslInit :: BS.ByteString -> BS.ByteString -> BS.ByteString -> St
saslInit un pw cn = St []
	[ ("username", un), ("authcid", un), ("password", pw), ("cnonce", cn) ]
