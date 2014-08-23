module XmppServer (
	Xmpp(..), fromCommon, Side(..), hlpDebug,
	Tags(..), Bind(..), Query(..), Tag(..),
	Feature(..), Requirement(..), runSasl,
	SaslError(..), SaslState(..), Jid(..),
	inputP2, inputP3, output,
	fromHandleLike, toHandleLike, voidM,
	SHandle(..),
	) where

import Xmpp
import SaslServer
