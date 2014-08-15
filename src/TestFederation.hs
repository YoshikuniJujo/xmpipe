{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module TestFederation (
	Common(..),
	Tag(..),
	Requirement(..),
	Feature(..),
	MBody(..),
	Jid(..),
	MessageType(..),
	input, output,
	inputSt, outputSt,
	) where

import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe
import Data.Pipe.Basic
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import XmppCommon

inputSt :: HandleLike h => h -> Pipe () Common (StateT s (HandleMonad h)) ()
inputSt h = handleSt h
	=$= xmlEvent
	=$= convert (myFromJust "here")
	=$= xmlReborn
	=$= convert toCommon
	=$= debugSt h

input :: HandleLike h => h -> Pipe () Common (HandleMonad h) ()
input h = handleP h
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlReborn
	=$= convert toCommon
	=$= debugP h

debugSt :: (HandleLike h, Show a) => h -> Pipe a a (StateT s (HandleMonad h)) ()
debugSt h = await >>= \mx -> case mx of
	Just x -> do
		lift . lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show x
		yield x
		debugSt h
	_ -> return ()

debugP :: (HandleLike h, Show a) => h -> Pipe a a (HandleMonad h) ()
debugP h = await >>= \mx -> case mx of
	Just x -> do
		lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show x
		yield x
		debugP h
	_ -> return ()

outputSt :: HandleLike h => h -> Pipe Common () (StateT s (HandleMonad h)) ()
outputSt h = do
	mn <- await
	case mn of
		Just n -> do
			lift . lift . hlPut h $ xmlString [fromCommon Server n]
			case n of
				XCEnd -> lift . lift $ hlClose h
				_ -> return ()
			outputSt h
		_ -> return ()

output :: HandleLike h => h -> Pipe Common () (HandleMonad h) ()
output h = do
	mn <- await
	case mn of
		Just n -> do
			lift . hlPut h $ xmlString [fromCommon Server n]
			case n of
				XCEnd -> lift $ hlClose h
				_ -> return ()
			output h
		_ -> return ()

handleSt :: (HandleLike h, MonadTrans t, Monad (t (HandleMonad h))) =>
	h -> Pipe () BS.ByteString (t (HandleMonad h)) ()
handleSt h = lift (lift $ hlGetContent h) >>= ((>> handleSt h) . yield)

handleP :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
handleP h = lift (hlGetContent h) >>= ((>> handleP h) . yield)

myFromJust :: String -> Maybe a -> a
myFromJust _ (Just x) = x
myFromJust msg _ = error msg
