{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module XmppServer (
	Xmpp(..), fromCommon, Side(..), hlpDebug,
	Bind(..), Query(..), Tag(..),
	Feature(..), Requirement(..), runSasl,
	SaslError(..), SaslState(..), Jid(..),
	input, inputP2, inputP3, output,
	fromHandleLike, toHandleLike, voidM,
	SHandle(..),

	Tags(..), tagsType,

	starttls,

	sasl,
	XmppState, initXmppState, receiver, nextUuid, setResource,
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Data.UUID

import Xmpp
import SaslServer

import qualified Data.ByteString as BS

starttls :: Monad m => Pipe BS.ByteString BS.ByteString m ()
starttls = inputP3 =$= processTls =$= output

processTls :: Monad m => Pipe Xmpp Xmpp m ()
processTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		yield XCDecl
		yield $ XCBegin [
			--	(Id, toASCIIBytes u),
			(Id, "83e074ac-c014-432e9f21-d06e73f5777e"),
			(From, "localhost"),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		yield $ XCFeatures [FtStarttls Required]
		processTls
	Just XCStarttls -> yield XCProceed
	Just _ -> error "processTls: bad"
	_ -> return ()

sasl :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m)) =>
	Pipe BS.ByteString BS.ByteString m ()
sasl = inputP2 =$= makeSasl =$= output

makeSasl :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m)) => Pipe Xmpp Xmpp m ()
makeSasl = (,) `liftM` await `ap` lift (gets receiver) >>= \p -> case p of
	(Just (XCBegin _), Nothing) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, toASCIIBytes u),
			(From, "localhost"),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		runSasl
	_ -> return ()

data XmppState = XmppState {
	receiver :: Maybe Jid,
	uuidList :: [UUID],
	saslState :: [(BS.ByteString, BS.ByteString)] }

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

nextUuid :: (MonadState m, StateType m ~ XmppState) => m UUID
nextUuid = do
	xs@XmppState { uuidList = u : us } <- get
	put xs { uuidList = us }
	return u

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
