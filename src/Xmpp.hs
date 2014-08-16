module Xmpp (
	input,

	Xmpp(..), fromCommon, Jid(..), toJid, Side(..),
	Feature(..), Tag(..),
	MessageType(..), IqType(..), Query(..), Roster(..),
	MBody(..),
	Bind(..), Requirement(..),

	voidM, hlpDebug, SHandle(..),
	) where

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
