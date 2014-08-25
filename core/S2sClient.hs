{-# LANGUAGE OverloadedStrings #-}

module S2sClient (
	Xmpp(..), Feature(..), Tag(..),

	sasl, SaslState(..),
	toHandleLike, fromHandleLike, outputS, inputP3, hlpDebug,
	SHandle(..),

	starttls,
	) where

import Data.Pipe

import qualified Data.ByteString as BS

import Tools
import SaslClient
import Xmpp

starttls :: Monad m => Pipe BS.ByteString BS.ByteString m ()
starttls = inputP3 =$= processTls =$= outputS

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = do
	yield XCDecl
	yield $ XCBegin [(From, "localhost"), (To, "otherhost"),
		(TagRaw $ nullQ "version", "1.0")]
	procTls

procTls :: Monad m => Pipe Xmpp Xmpp m ()
procTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> procTls
	Just (XCFeatures [FtStarttls _]) -> do
		yield XCStarttls
		procTls
	Just XCProceed -> return ()
	Just _ -> return ()
	_ -> return ()
