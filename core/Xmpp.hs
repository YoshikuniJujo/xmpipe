{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Xmpp (
	input, input', input'', output,
	inputC2, inputC3, inputC, outputC,
	inputP2, inputP3, outputP,

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
inputC2 c = fromChan c =$= inputP2

input :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) ()
input h = fromHandleLike h =$= inputP2

inputP2 :: Monad m => Pipe BS.ByteString Xmpp m ()
inputP2 = xmlEvent =$= convert fromJust =$= mapOut toCommon xmlReborn

isSaslSuccess :: XmlNode -> Bool
isSaslSuccess (XmlNode ((_, Just "jabber:client"), "iq")
	_ _ [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ _ _]) = True
isSaslSuccess _ = False

inputC3 :: MonadBaseControl IO m => TChan BS.ByteString -> Pipe () Xmpp m [Xmlns]
inputC3 c = fromChan c =$= inputP3

inputP3 :: Monad m => Pipe BS.ByteString Xmpp m [Xmlns]
inputP3 = xmlEvent =$= convert fromJust =$= mapOut toCommon xmlPipe

input' :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) [Xmlns]
input' h = fromHandleLike h =$= inputP3

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
output h = doIf (== XCEnd) (hlClose h) =$= outputP =$= toHandleLike h

toHandleLike :: HandleLike h => h -> Pipe BS.ByteString () (HandleMonad h) ()
toHandleLike h = await >>= maybe (return ()) ((>> toHandleLike h) . lift . hlPut h)

doIf :: Monad m => (a -> Bool) -> m () -> Pipe a a m ()
doIf p m = (await >>=) . maybe (return ()) $ \x ->
	(>> doIf p m) $ if p x then lift m else yield x

outputC :: MonadBaseControl IO m => TChan BS.ByteString -> Pipe Xmpp () m ()
outputC c = outputP =$= toChan c

outputP :: Monad m => Pipe Xmpp BS.ByteString m ()
outputP = convert $ xmlString . (: []) . fromCommon Client
