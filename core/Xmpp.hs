{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Xmpp (
	input, input', input'', output,

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
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import XmppType
import Tools

tagsNull :: Tags
tagsNull = Tags Nothing Nothing Nothing Nothing Nothing []

input :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) ()
input h = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
	=$= mapOut toCommon xmlReborn

isSaslSuccess :: XmlNode -> Bool
isSaslSuccess (XmlNode ((_, Just "jabber:client"), "iq")
	_ _ [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ _ _]) = True
isSaslSuccess _ = False

input' :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) [Xmlns]
input' h = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
--	=$= hlpDebug h
	=$= mapOut toCommon xmlPipe

xmlPipe :: Monad m => Pipe XmlEvent XmlNode m [Xmlns]
xmlPipe = xmlBegin >>= \ns -> xmlNodeUntil isSaslSuccess ns >> return ns

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
