{-# LANGUAGE PackageImports #-}

module Xmpp (
	input, output,

	Xmpp(..), fromCommon, Jid(..), toJid, Side(..),
	Feature(..), Tag(..),
	MessageType(..), IqType(..), Query(..), Roster(..),
	MBody(..),
	Bind(..), Requirement(..),

	voidM, hlpDebug, SHandle(..),
	) where

import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.Pipe
import Data.Pipe.Basic
import Data.HandleLike
import Text.XML.Pipe

import XmppType
import Tools

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