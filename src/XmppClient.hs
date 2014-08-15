{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections,
	PackageImports, FlexibleContexts #-}

module XmppClient (
	Common(..),
	Jid(..), toJid,
	IqType(..),
	Tag(..),
	MessageType(..),
	MBody(..),
	MessageBody(..),
	Query(..),
	input, output,
	sasl,
	XmppState(..),
	Feature(..),
	handleP,
	voidM,
	external,
	convert,
	SHandle(..),
	Bind(..),
	) where

import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import SaslClient

import XmppCommon

data SHandle s h = SHandle h

instance HandleLike h => HandleLike (SHandle s h) where
	type HandleMonad (SHandle s h) = StateT s (HandleMonad h)
	type DebugLevel (SHandle s h) = DebugLevel h
	hlPut (SHandle h) = lift . hlPut h
	hlGet (SHandle h) = lift . hlGet h
	hlClose (SHandle h) = lift $ hlClose h
	hlDebug (SHandle h) = (lift .) . hlDebug h

data XmppState = XmppState [(BS.ByteString, BS.ByteString)] deriving Show

instance SaslState XmppState where
	getSaslState (XmppState ss) = ss
	putSaslState ss _ = XmppState ss

input :: HandleLike h => h -> Pipe () Common (HandleMonad h) ()
input h = handleP h
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlPipe
	=$= checkP h
	=$= convert toCommon
	=$= checkSR h

checkP :: HandleLike h => h -> Pipe XmlNode XmlNode (HandleMonad h) ()
checkP h = do
	mn <- await
	case mn of
		Just n@(XmlStart (_, "stream") _ _) ->
			lift (hlDebug h "critical" $ showBS n) >>
				yield n >> checkP h
		Just n@(XmlNode (_, "challenge") _ _ [XmlCharData cd]) ->
			lift (hlDebug h "critical" . (`BS.append` "\n\n") .
					(\(Right s) -> s) $ B64.decode cd) >>
				yield n >> checkP h
		Just n -> yield n >> checkP h
		_ -> return ()

checkSR :: HandleLike h => h -> Pipe Common Common (HandleMonad h) ()
checkSR h = do
	mr <- await
	case mr of
		Just r -> lift (hlDebug h "critical" . (`BS.append` "\n") $
			showSR r) >> yield r >> checkSR h
		_ -> return ()

voidM :: Monad m => m a -> m ()
voidM = (>> return ())

xmlPipe :: Monad m => Pipe XmlEvent XmlNode m ()
xmlPipe = do
	c <- xmlBegin >>= xmlNode
	when c xmlPipe

output :: HandleLike h => h -> Pipe Common () (HandleMonad h) ()
output h = do
	mn <- await
	case mn of
		Just n -> do
			lift (hlPut h $ xmlString [fromCommon Client n])
			case n of
				XCEnd -> lift $ hlClose h
				_ -> return ()
			output h
		_ -> return ()

handleP :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
handleP h = do
	c <- lift $ hlGetContent h
	yield c
	handleP h

showSR :: Common -> BS.ByteString
showSR (XCMessage Chat i f t (MBodyRaw ns))
	| Just dm <- toDelayedMessage ns =
		BSC.pack . (++ "\n") $ show ("CHAT" :: String, i, f, t, dm)
showSR rs = BSC.pack . (++ "\n") $ show rs

showBS :: Show a => a -> BS.ByteString
showBS rs = BSC.pack . (++ "\n") $ show rs

external :: Monad m => Pipe Common Common m ()
external = do
	yield $ XCAuth "EXTERNAL" Nothing
	mr <- await
	case mr of
		Just (SRChallenge "") -> yield $ SRResponse ""
		_ -> error $ "external: bad " ++ show mr
