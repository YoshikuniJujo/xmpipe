{-# LANGUAGE PackageImports #-}

module Xmpp (
	input, output,

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
	=$= xmlReborn
	=$= convert toCommon

output :: HandleLike h => h -> Pipe Xmpp () (HandleMonad h) ()
output h = (await >>=) . maybe (return ()) $ \n -> (>> output h) $ do
	lift (hlPut h $ xmlString [fromCommon Client n])
	case n of XCEnd -> lift $ hlClose h; _ -> return ()
