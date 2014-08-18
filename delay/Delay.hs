{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Delay (processDelayed, DelayedMessage, toDelayedMessage) where

import Control.Arrow
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Text.XML.Pipe
import XmppType

import qualified Data.ByteString as BS

type Delayed = (BS.ByteString, BS.ByteString, Maybe Jid, Jid, DelayedMessage)

processDelayed :: (Monad m,
--	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) =>
	(Delayed -> m ()) -> Pipe Xmpp Xmpp m ()
processDelayed p = convert readDelay =$= procDelayed p

procDelayed :: (Monad m,
--	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) =>
	(Delayed -> m ()) -> Pipe (Either a Delayed) a m ()
procDelayed p = await >>= \ed -> case ed of
	Just (Left x) -> yield x >> procDelayed p
	Just (Right d) -> lift (p d) >> procDelayed p
	_ -> return ()

readDelay :: Xmpp -> Either Xmpp Delayed
readDelay (XCMessage Chat i f t (MBodyRaw ns))
	| Just dm <- toDelayedMessage ns = Right ("CHAT", i, f, t, dm)
readDelay x = Left x

data DelayedMessage
	= MBodyDelay MessageBody MessageDelay MessageXDelay
	deriving Show

toDelayedMessage :: [XmlNode] -> Maybe DelayedMessage
toDelayedMessage [b, d, xd]
	| XmlNode ((_, Just q), "body") _ [] _ <- b,
		XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ _ [] <- d,
		XmlNode ((_, Just "jabber:x:delay"), "x") _ _ [] <- xd,
		q `elem` ["jabber:client", "jabber:server"] = Just $
		MBodyDelay (toBody b) (toDelay d) (toXDelay xd)
toDelayedMessage _ = Nothing

data MessageDelay
	= MessageDelay [(DelayTag, BS.ByteString)]
	| MDRaw XmlNode
	deriving Show

data DelayTag = DTFrom | DTStamp | DlyTRaw QName deriving Show

data MessageXDelay
	= MessageXDelay [(XDelayTag, BS.ByteString)]
	| MXDRaw XmlNode
	deriving Show

data XDelayTag = XDTFrom | XDTStamp | XDlyTRaw QName deriving Show

toXDelay :: XmlNode -> MessageXDelay
toXDelay (XmlNode ((_, Just "jabber:x:delay"), "x") _ as []) =
	MessageXDelay $ map (first toXDelayTag) as
toXDelay n = MXDRaw n

toXDelayTag :: QName -> XDelayTag
toXDelayTag ((_, Just "jabber:x:delay"), "from") = XDTFrom
toXDelayTag ((_, Just "jabber:x:delay"), "stamp") = XDTStamp
toXDelayTag n = XDlyTRaw n

toDelayTag :: QName -> DelayTag
toDelayTag ((_, Just "urn:xmpp:delay"), "from") = DTFrom
toDelayTag ((_, Just "urn:xmpp:delay"), "stamp") = DTStamp
toDelayTag n = DlyTRaw n

toDelay :: XmlNode -> MessageDelay
toDelay (XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ as []) = MessageDelay $
	map (first toDelayTag) as
toDelay n = MDRaw n

data MessageBody
	= MessageBody BS.ByteString
	| MBRaw XmlNode
	deriving Show

toBody :: XmlNode -> MessageBody
toBody (XmlNode ((_, Just "jabber:client"), "body") _ [] [XmlCharData b]) =
	MessageBody b
toBody (XmlNode ((_, Just "jabber:server"), "body") _ [] [XmlCharData b]) =
	MessageBody b
toBody n = MBRaw n
