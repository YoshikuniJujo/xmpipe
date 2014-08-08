{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}

module XmppServer (
	MBody(..),
	MessageBody(..),
	Common(..),
	convert,
	nullQ,
	handleP,
	checkP,
	digestMd5,
	toCommon, fromCommon,
	Jid(..),
	MessageType(..),
	IqType(..),
	Query(..),
	Roster(..),
	Tag(..),
	Bind(..),
	Requirement(..),
	Mechanism(..),
	Feature(..),
	XmppState(..), initXmppState,
		setReceiver, setResource, nextUuid,
	input,
	output,
	) where

import Control.Concurrent.STM
import TestFederationCl

import Data.UUID

import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import DigestSv

import XmppCommon

data SHandle s h = SHandle h

instance HandleLike h => HandleLike (SHandle s h) where
	type HandleMonad (SHandle s h) = StateT s (HandleMonad h)
	type DebugLevel (SHandle s h) = DebugLevel h
	hlPut (SHandle h) = lift . hlPut h
	hlGet (SHandle h) = lift . hlGet h
	hlClose (SHandle h) = lift $ hlClose h
	hlDebug (SHandle h) = (lift .) . hlDebug h

data XmppState = XmppState {
	receiver :: Maybe Jid,
	uuidList :: [UUID] }

initXmppState :: [UUID] -> XmppState
initXmppState uuids = XmppState {
	receiver = Nothing,
	uuidList = uuids }

setReceiver :: Jid -> XmppState -> XmppState
setReceiver j xs = xs { receiver = Just j }

setResource :: BS.ByteString -> XmppState -> XmppState
setResource r xs@XmppState{ receiver = Just (Jid a d _) } =
	xs { receiver = Just . Jid a d $ Just r }
setResource _ _ = error "setResource: can't set resource to Nothing"

nextUuid :: (MonadState m, StateType m ~ XmppState) => m UUID
nextUuid = do
	xs@XmppState { uuidList = u : us } <- get
	put xs { uuidList = us }
	return u

output :: (MonadIO (HandleMonad h),
	MonadState (HandleMonad h), StateType (HandleMonad h) ~ XmppState,
	HandleLike h) =>
	TVar [(String, TChan Common)] -> h -> Pipe Common () (HandleMonad h) ()
output sl h = do
	mx <- await
	case mx of
		Just m@(XCMessage Chat _ _ (Jid "yoshio" "otherhost" Nothing) _)
			-> do	lift (hlDebug h "critical" "HERE")
				l <- liftIO . atomically $ readTVar sl
				case lookup "otherhost" l of
					Just i -> liftIO . atomically . writeTChan i $
						convertMessage m
					_ -> otherhost sl m
				output sl h
		Just x -> lift (hlPut h $ xmlString [fromCommon Client x]) >> output sl h
		_ -> return ()

otherhost :: MonadIO m =>
	TVar [(String, TChan Common)] -> Common -> Pipe Common () m ()
otherhost sl m = liftIO $ do
	(ca, k, c) <- readFiles
	(i, e) <- connect ca k c
	atomically . writeTChan i $ convertMessage m
	atomically $ readTChan e
	atomically $ modifyTVar sl (("otherhost", i) :)

input :: HandleLike h => h -> Pipe () Common (HandleMonad h) ()
input h = handleP h
	=$= xmlEvent
--	=$= checkP h
	=$= convert fromJust
	=$= xmlPipe
	=$= convert toCommon
	=$= checkP h

xmlPipe :: Monad m => Pipe XmlEvent XmlNode m ()
xmlPipe = xmlBegin >>= xmlNode >>= flip when xmlPipe

handleP :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
handleP h = do
	c <- lift $ hlGetContent h
	yield c
	handleP h

checkP :: (HandleLike h, Show a) => h -> Pipe a a (HandleMonad h) ()
checkP h = do
	mx <- await
	case mx of
		Just x -> do
			lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show x
			yield x
			checkP h
		_ -> return ()

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) (\x -> yield (f x) >> convert f)

digestMd5 :: (MonadState m, StateType m ~ XmppState) =>
	Maybe BS.ByteString -> UUID -> Pipe Common Common m BS.ByteString
digestMd5 e u = do
	yield $ XCFeatures
		[FtMechanisms $ (if isJust e then (External :) else id) [DigestMd5]]
	a <- await
	case (a, e) of
		(Just (XCAuth DigestMd5), _) -> digestMd5Body u
		(Just (XCAuth External), Just n) -> external n
		_ -> error $ "BAD: " ++ show a

external :: Monad m => BS.ByteString -> Pipe Common Common m BS.ByteString
external e = do
	yield SRChallengeNull
	Just SRResponseNull <- await
	yield XCSaslSuccess
	return e

digestMd5Body :: (MonadState m, StateType m ~ XmppState) =>
	UUID -> Pipe Common Common m BS.ByteString
digestMd5Body u = do
	yield SRChallenge {
		realm = "localhost",
		nonce = toASCIIBytes u,
		qop = "auth",
		charset = "utf-8",
		algorithm = "md5-sess" }
	Just (SRResponse r dr@DR { drUserName = un }) <- await
	let cret = fromJust . lookup "response" $ responseToKvs True dr
	unless (r == cret) $ error "digestMd5: bad authentication"
	let sret = B64.encode . ("rspauth=" `BS.append`) . fromJust
		. lookup "response" $ responseToKvs False dr
	yield $ SRChallengeRspauth sret
	Just SRResponseNull <- await
	yield XCSaslSuccess
	return un
