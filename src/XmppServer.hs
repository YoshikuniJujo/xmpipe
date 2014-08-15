{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}

module XmppServer (
	SaslError(..),
	Jid(..),
	MessageType(..),
	Xmpp(..),
	MBody(..),
	Roster(..),
	Query(..),
	IqType(..),
	Bind(..),
	setResource,
	Requirement(..),
	Tag(..),
	Feature(..),
	nextUuid,
	input,
	XmppState(..),
	initXmppState,
	runSasl,
	SHandle(..),
	fromHandleLike,
	hlpDebug,
	voidM,

	fromCommon,
	Side(..),
	) where

import Data.UUID

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.Pipe
import Data.Pipe.Basic
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS

import Tools
import XmppType
import SaslServer

data XmppState = XmppState {
	receiver :: Maybe Jid,
	uuidList :: [UUID],
	saslState :: [(BS.ByteString, BS.ByteString)] }

initXmppState :: [UUID] -> XmppState
initXmppState uuids = XmppState {
	receiver = Nothing,
	uuidList = uuids,
	saslState = [
		("realm", "localhost"),
		("nonce", "7658cddf-0e44-4de2-87df-4132bce97f4"),
		("qop", "auth"),
		("charset", "utf-8"),
		("algorithm", "md5-sess"),
		("snonce", "7658cddf-0e44-4de2-87df-4132bce97f4") ] }

setResource :: BS.ByteString -> XmppState -> XmppState
setResource r xs@XmppState{ receiver = Just (Jid a d _) } =
	xs { receiver = Just . Jid a d $ Just r }
setResource _ _ = error "setResource: can't set resource to Nothing"

nextUuid :: (MonadState m, StateType m ~ XmppState) => m UUID
nextUuid = do
	xs@XmppState { uuidList = u : us } <- get
	put xs { uuidList = us }
	return u

input :: HandleLike h => h -> Pipe () Xmpp (HandleMonad h) ()
input h = fromHandleLike h
	=$= xmlEvent
	=$= convert fromJust
	=$= xmlReborn
	=$= convert toCommon
	=$= hlpDebug h

runSasl :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m) ) => Pipe Xmpp Xmpp m ()
runSasl = do
	yield $ XCFeatures [FtMechanisms ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]]
	await >>= \a -> case a of
		Just (XCAuth m i) -> sasl m i
		_ -> throwError $ fromSaslError
			(SaslErrorType "EOF") "unexpected EOF"

sasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	BS.ByteString -> Maybe BS.ByteString -> Pipe Xmpp Xmpp m ()
sasl n i = case lookup n saslServers of
	Just (b, s) -> saslPipe b i s
	_ -> throwError $ fromSaslError InvalidMechanism "no such mechanisms"

saslPipe :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => Bool
	-> Maybe BS.ByteString
	-> Pipe BS.ByteString (Either Success BS.ByteString) m ()
	-> Pipe Xmpp Xmpp m ()
saslPipe True (Just i) s =
	(yield i >> convert (\(SRResponse r) -> r)) =$= s =$= saslOutput
saslPipe True _ s = convert (\(SRResponse r) -> r) =$= s
		=$= (yield (SRChallenge "") >> saslOutput)
saslPipe False Nothing s = convert (\(SRResponse r) -> r) =$= s =$= saslOutput
saslPipe _ _ _ = throwError $
	fromSaslError MalformedRequest "no need of initial data"

saslOutput :: (MonadState m, SaslState (StateType m)) =>
	Pipe (Either Success BS.ByteString) Xmpp m ()
saslOutput = await >>= \mch -> case mch of
	Just (Right r) -> yield (SRChallenge r) >> saslOutput
	Just (Left (SaslServer.Success r)) -> yield $ XCSaslSuccess r
	_ -> return ()

instance SaslState XmppState where
	getSaslState xs = case receiver xs of
		Just (Jid un _ _) -> ("username", un) : ss'
		_ -> ss'
		where
		ss' = let u : _ = uuidList xs in ("uuid", toASCIIBytes u) : ss
		ss = saslState xs
	putSaslState ss xs = case lookup "username" ss of
		Just un -> case receiver xs of
			Just (Jid _ d r) -> xs' { receiver = Just $ Jid un d r }
			_ -> xs' { receiver = Just $ Jid un "localhost" Nothing }
		_ -> xs'
		where
		xs' = xs {uuidList = tail $ uuidList xs, saslState = ss}
