{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Delay (readDelay) where

-- import Control.Arrow
import Text.XML.Pipe
import XmppType

import qualified Data.ByteString as BS

type Delayed = (BS.ByteString, BS.ByteString, Maybe Jid, Jid, DelayedMessage)

readDelay :: Xmpp -> Either Delayed Xmpp
{-
readDelay (XCMessage Chat i f t (MBodyRaw ns))
	| Just dm <- toDelayedMessage ns = Left ("CHAT", i, f, t, dm)
	-}
readDelay x = Right x

data DelayedMessage
	= MBodyDelay MessageBody MessageDelay MessageXDelay
	| MBodyDelayRaw [XmlNode]
	deriving Show

{-
toDelayedMessage :: [XmlNode] -> Maybe DelayedMessage
toDelayedMessage ns = Just $ MBodyDelayRaw ns
toDelayedMessage [b, d, xd]
	| XmlNode ((_, Just q), "body") _ [] _ <- b,
		XmlNode ((_, Just "urn:xmpp:delay"), "delay") _ _ [] <- d,
		XmlNode ((_, Just "jabber:x:delay"), "x") _ _ [] <- xd,
		q `elem` ["jabber:client", "jabber:server"] = Just $
		MBodyDelay (toB b) (toDelay d) (toXDelay xd)
toDelayedMessage _ = Nothing
-}

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

data MessageBody
	= MessageBody BS.ByteString
	| MBRaw XmlNode
	deriving Show

{-
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

toB :: XmlNode -> MessageBody
toB (XmlNode ((_, Just "jabber:client"), "body") _ [] [XmlCharData b]) =
	MessageBody b
toB (XmlNode ((_, Just "jabber:server"), "body") _ [] [XmlCharData b]) =
	MessageBody b
toB n = MBRaw n
-}
