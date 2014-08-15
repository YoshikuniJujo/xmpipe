{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections,
	PackageImports, FlexibleContexts #-}

module XmppClient (
	Xmpp(..),
	Jid(..), toJid,
	IqType(..),
	Tag(..),
	MessageType(..),
	MBody(..),
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

import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import SaslClient

import XmppType

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

input :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) ()
input h = handleP h
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlReborn
	=$= checkP h
	=$= convert toCommon

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

voidM :: Monad m => m a -> m ()
voidM = (>> return ())

output :: HandleLike h => h -> Pipe Xmpp () (HandleMonad h) ()
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

showBS :: Show a => a -> BS.ByteString
showBS rs = BSC.pack . (++ "\n") $ show rs

external :: Monad m => Pipe Xmpp Xmpp m ()
external = do
	yield $ XCAuth "EXTERNAL" Nothing
	mr <- await
	case mr of
		Just (SRChallenge "") -> yield $ SRResponse ""
		_ -> error $ "external: bad " ++ show mr
