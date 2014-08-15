{-# LANGUAGE OverloadedStrings #-}

module Delay (toDelayedMessage) where

import Control.Arrow
import Text.XML.Pipe

import qualified Data.ByteString as BS

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
