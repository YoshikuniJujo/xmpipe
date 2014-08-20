{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module XmppClient (
	Jid(..), toJid, fromJid,
	Tags(..),
	Xmpp(..), Feature(..),
	Query(..), Bind(..),
	Tag(..),
	SASL.SaslState(..),

	Requirement(..), toRequirement,

	input, input', input'', output, begin, starttls, sasl,

	tagsNull,
	) where

import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.HandleLike
import Data.Pipe

import qualified Data.ByteString as BS

import Xmpp
import qualified SaslClient as SASL

begin :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
begin h l = do
	yield XCDecl
	yield $ XCBegin [(To, h), (TagRaw $ nullQ "version", "1.0"), (Lang, l)]

starttls :: HandleLike h => h -> BS.ByteString -> HandleMonad h ()
starttls h hst = voidM . runPipe $ input h
--	=$= hlpDebug h
	=$= (begin hst "en" >> starttls_) =$= output h

starttls_ :: Monad m => Pipe Xmpp Xmpp m ()
starttls_ = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	when (null $ filter isSt fs) $ fail "starttls_: not support tls"
	yield XCStarttls
	Just XCProceed <- await
	return ()
	where isSt (FtStarttls _) = True; isSt _ = False

sasl :: (
	HandleLike h,
	MonadState (HandleMonad h), SASL.SaslState (StateType (HandleMonad h)),
	MonadError (HandleMonad h), Error (ErrorType (HandleMonad h)) ) =>
	h -> BS.ByteString -> [BS.ByteString] -> HandleMonad h ()
sasl h hst ms = voidM . runPipe $ input h
--	=$= hlpDebug h
	=$= (begin hst "en" >> sasl_ ms) =$= output h

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
