{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Xmpp (
	input, input', input'', output,
	inputC2, inputC3, inputC, outputC,

	Xmpp(..), fromCommon,
	Tags(..),
	Jid(..), toJid, fromJid,
	Side(..),
	Feature(..), Tag(..),
	Query(..), Bind(..), Requirement(..), toRequirement, fromRequirement,

	voidM, hlpDebug, SHandle(..),

	nullQ, tagsNull,
	) where

import "monads-tf" Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Concurrent.STM
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS

import XmppType
import Tools
import ManyChanPipe

tagsNull :: Tags
tagsNull = Tags Nothing Nothing Nothing Nothing Nothing []

inputC2 :: MonadBaseControl IO m => TChan BS.ByteString -> Pipe () Xmpp m ()
inputC2 c = fromChan c
	=$= xmlEvent
	=$= convert fromJust
	=$= mapOut toCommon xmlReborn

input :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) ()
input h = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
	=$= mapOut toCommon xmlReborn

isSaslSuccess :: XmlNode -> Bool
isSaslSuccess (XmlNode ((_, Just "jabber:client"), "iq")
	_ _ [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ _ _]) = True
isSaslSuccess _ = False

inputC3 :: MonadBaseControl IO m => TChan BS.ByteString -> Pipe () Xmpp m [Xmlns]
inputC3 c = fromChan c
	=$= xmlEvent
	=$= convert fromJust
--	=$= hlpDebug h
	=$= mapOut toCommon xmlPipe

input' :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) [Xmlns]
input' h = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
--	=$= hlpDebug h
	=$= mapOut toCommon xmlPipe

xmlPipe :: Monad m => Pipe XmlEvent XmlNode m [Xmlns]
xmlPipe = xmlBegin >>= \ns -> xmlNodeUntil isSaslSuccess ns >> return ns

inputC :: MonadBaseControl IO m =>
	TChan BS.ByteString -> [Xmlns] -> Pipe () Xmpp m ()
inputC c ns = fromChan c
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlNode ns
	=$= convert toCommon

input'' :: HandleLike h => h -> [Xmlns] -> Pipe () Xmpp (HandleMonad h) ()
input'' h ns = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlNode ns
	=$= convert toCommon

output :: HandleLike h => h -> Pipe Xmpp () (HandleMonad h) ()
output h = (await >>=) . maybe (return ()) $ \n -> (>> output h) $ do
	lift (hlPut h $ xmlString [fromCommon Client n])
	case n of XCEnd -> lift $ hlClose h; _ -> return ()

outputC :: MonadBaseControl IO m => TChan BS.ByteString -> Pipe Xmpp () m ()
outputC c = (await >>=) . maybe (return ()) $ \n -> (>> outputC c) $ do
	lift (liftBase . atomically . writeTChan c $ xmlString [fromCommon Client n])
