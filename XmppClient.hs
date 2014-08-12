{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections,
	PackageImports, FlexibleContexts #-}

module XmppClient (
	XmppState(..),
	fromCaps,
	toCaps,
	XmlNode(..),
	nullQ,
	toJid,
	MBody(..),
	capsToXmlCaps,
	capsToQuery,
	Common(..),
	handleP,
	convert,
	external,
	digestMd5,
	scramSha1,
	SHandle(..),
	input, output,
	Query(..),
	DiscoTag(..),
	XmlCaps(..),
	CapsTag(..),
	Tag(..),
	Bind(..),
	Feature(..),
	Mechanism(..),
	Requirement(..),
	MessageXDelay(..),
	MessageDelay(..),
	MessageBody(..),
	InfoFeature(..),
	InfoFeatureTag(..),
	Identity(..),
	IdentityTag(..),
	DelayTag(..),
	XDelayTag(..),
	voidM,
	MessageType(..),
	Jid(..),
	IqType(..),
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

import Digest

import XmppCommon
import Caps hiding (Identity)

data SHandle s h = SHandle h

instance HandleLike h => HandleLike (SHandle s h) where
	type HandleMonad (SHandle s h) = StateT s (HandleMonad h)
	type DebugLevel (SHandle s h) = DebugLevel h
	hlPut (SHandle h) = lift . hlPut h
	hlGet (SHandle h) = lift . hlGet h
	hlClose (SHandle h) = lift $ hlClose h
	hlDebug (SHandle h) = (lift .) . hlDebug h

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
			showBS r) >> yield r >> checkSR h
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

showBS :: Show a => a -> BS.ByteString
showBS = BSC.pack . (++ "\n") . show

external :: Monad m => Pipe Common Common m ()
external = do
	yield $ XCAuth "EXTERNAL" Nothing
	mr <- await
	case mr of
		Just (SRChallenge "") -> yield $ SRResponse ""
		_ -> error $ "external: bad " ++ show mr

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) ((>> convert f) . yield . f)

data XmppState = XmppState [(BS.ByteString, BS.ByteString)] deriving Show

instance SaslState XmppState where
	getSaslState (XmppState ss) = ss
	putSaslState ss _ = XmppState ss

digestMd5 :: (Monad m, MonadState m, StateType m ~ XmppState) =>
	Pipe Common Common m ()
digestMd5 = saslPipe digestMd5Cl

scramSha1 :: (Monad m, MonadState m, StateType m ~ XmppState) =>
	Pipe Common Common m ()
scramSha1 = saslPipe scramSha1Cl

saslPipe :: (Monad m, MonadState m, StateType m ~ XmppState) =>
	(BS.ByteString, (Pipe BS.ByteString BS.ByteString m (), Bool))
		-> Pipe Common Common m ()
saslPipe m =
	inputScramSha1 =$= fst (snd m) =$= outputScramSha1 (snd (snd m)) (fst m)

inputScramSha1 :: Monad m => Pipe Common BS.ByteString m ()
inputScramSha1 = await >>= \mc -> case mc of
	Just (SRChallenge c) -> yield c >> inputScramSha1
	Just (XCSaslSuccess (Just d)) -> yield d
	Just (XCSaslSuccess _) -> yield ""
	_ -> error "inputScramSha1: bad"

outputScramSha1 :: Monad m =>
	Bool -> BS.ByteString -> Pipe BS.ByteString Common m ()
outputScramSha1 ci mn = do
	if ci
	then await >>= maybe (return ()) (yield . XCAuth mn . Just)
	else yield $ XCAuth mn Nothing
	convert SRResponse
