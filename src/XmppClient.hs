{-# LANGUAGE OverloadedStrings #-}

module XmppClient (
	Jid(..), toJid,
	Xmpp(..), Feature(..),
	MessageType(..), MBody(..), IqType(..), Query(..), Bind(..),
	Tag(..),

	input, output, SASL.sasl, begin, starttls,
	) where

import Control.Monad
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
