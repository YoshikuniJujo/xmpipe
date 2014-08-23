{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module XmppServer (
	Xmpp(..), fromCommon, Side(..), hlpDebug,
	Tags(..), Bind(..), Query(..), Tag(..),
	Feature(..), Requirement(..), runSasl,
	SaslError(..), SaslState(..), Jid(..),
	inputP2, inputP3, output,
	fromHandleLike, toHandleLike, voidM,
	SHandle(..),

	starttls,
	) where

import Data.Pipe

import Xmpp
import SaslServer

import qualified Data.ByteString as BS

starttls :: Monad m => Pipe BS.ByteString BS.ByteString m ()
starttls = inputP3 =$= processTls =$= output

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		yield XCDecl
		yield $ XCBegin [
			--	(Id, toASCIIBytes u),
			(Id, "83e074ac-c014-432e9f21-d06e73f5777e"),
			(From, "localhost"),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		yield $ XCFeatures [FtStarttls Required]
		processTls
	Just XCStarttls -> yield XCProceed
	Just _ -> error "processTls: bad"
	_ -> return ()
