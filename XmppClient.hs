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

data XmppState = XmppState [(BS.ByteString, BS.ByteString)]

instance SaslState XmppState where
	getSaslState (XmppState ss) = ss
	putSaslState ss _ = XmppState ss

digestMd5 :: (Monad m, MonadState m, StateType m ~ XmppState) =>
	Pipe Common Common m ()
digestMd5 = do
	yield $ XCAuth "DIGEST-MD5" Nothing
	convert (\(SRChallenge c) -> c) =$= digestMd5Cl =$= convert SRResponse

scramSha1 :: (Monad m, MonadState m, StateType m ~ XmppState) =>
	Pipe Common Common m ()
scramSha1 = do
	s <- lift scramInitCl
	yield . XCAuth "SCRAM-SHA-1" $ Just s
--	yield . XCAuth "SCRAM-SHA-1" $ Just
--		"n,,n=yoshikuni,r=00DEADBEEF00"
--	await >>= maybe (return ()) (\(SRChallenge "") -> return ())
	convert (\(SRChallenge c) -> c) =$= scramSha1Cl =$= convert SRResponse
--	yield $ SRResponse ""

inputScramSha1 :: Monad m => BS.ByteString -> Pipe Common BS.ByteString m ()
inputScramSha1 i = do
	yield i
	convert (\(SRChallenge c) -> c)

{-
outputScramSha1 :: Pipe BS.ByteString (Either BS.ByteString Common) m ()
outputScramSha1
-}
