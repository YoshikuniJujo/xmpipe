{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module XmppClient (
	Jid(..), toJid, fromJid,
	Xmpp(..), Feature(..),
	MessageType(..), MBody(..), Query(..), Bind(..),
	Tag(..),
	SASL.SaslState(..),

	Requirement(..), toRequirement,

	input, output, begin, starttls, sasl,
	) where

import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe

import qualified Data.ByteString as BS

import Xmpp
import qualified SaslClient as SASL

begin :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
begin h l = do
	yield XCDecl
	yield $ XCBegin [(To, h), (Version, "1.0"), (Lang, l)]

starttls :: Monad m => Pipe Xmpp Xmpp m ()
starttls = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	when (null $ filter isSt fs) $ fail "starttls: not support tls"
	yield XCStarttls
	Just XCProceed <- await
	return ()
	where isSt (FtStarttls _) = True; isSt _ = False

sasl :: (Monad m, MonadState m, SASL.SaslState (StateType m), MonadError m, Error (ErrorType m) ) =>
	[BS.ByteString] -> Pipe Xmpp Xmpp m ()
sasl sl = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	let	Just (FtMechanisms ms) = find isFtMechanisms fs
		Just n = listToMaybe $ sl `intersect` ms
	SASL.sasl n
	where
	isFtMechanisms (FtMechanisms _) = True
	isFtMechanisms _ = False
