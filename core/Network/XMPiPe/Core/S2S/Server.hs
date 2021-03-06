{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

module Network.XMPiPe.Core.S2S.Server (
	-- * Types and Values
	Mpi(..), XmppState(..), Tags(..), tagsNull, tagsType, SaslState, SaslError,
	-- * Functions
	starttls, sasl, begin, input, output,
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.Pipe
import Text.XML.Pipe

import Xmpp hiding (input, output)
import SaslServer

import qualified Data.ByteString as BS

input :: Monad m => [Xmlns] -> Pipe BS.ByteString Mpi m ()
input = inputMpi

output :: Monad m => Pipe Mpi BS.ByteString m ()
output = outputMpi

starttls :: (MonadState m, XmppState ~ StateType m) =>
	Pipe BS.ByteString BS.ByteString m ()
starttls = inputP3 =$= processTls =$= outputS

processTls :: (MonadState m, XmppState ~ StateType m) => Pipe Xmpp Xmpp m ()
processTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		yield XCDecl
		nextUuid >>= yield . begin_
		yield $ XCFeatures [FtStarttls Required]
		processTls
	Just XCStarttls -> yield XCProceed
	_ -> return ()

begin_ :: BS.ByteString -> Xmpp
begin_ u = XCBegin [
	(From, "otherhost"), (To, "localhost"),
	(TagRaw $ nullQ "version", "1.0"), (Id, u) ]

nextUuid :: (MonadState m, StateType m ~ XmppState) => Pipe a b m BS.ByteString
nextUuid = lift $ do
	u <- gets $ head . xsUuid
	modify dropUuid
	return u

data XmppState = XmppState {
	xsDomainName :: Maybe BS.ByteString,
	xsUuid :: [BS.ByteString] }
	deriving Show

instance SaslState XmppState where
	getSaslState _ = [("username", "")]
	putSaslState _ = id

dropUuid :: XmppState -> XmppState
dropUuid xs = xs { xsUuid = tail $ xsUuid xs }

sasl :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m) ) =>
	(BS.ByteString -> Bool) -> Pipe BS.ByteString BS.ByteString m ()
sasl rt = inputP2 =$= processSasl rt' =$= outputS
	where
	rt' n = if rt n then return () else
		throwError $ fromSaslError NotAuthorized n

processSasl :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m) ) =>
	(BS.ByteString -> m ()) -> Pipe Xmpp Xmpp m ()
processSasl rt = await >>= \mx -> case mx of
	Just (XCBegin as) -> do
		modify $ \st -> st { xsDomainName = lookup From as }
		yield XCDecl
		nextUuid >>= yield . begin_
		yield $ XCFeatures [FtMechanisms ["EXTERNAL"]]
		Just (XCAuth "EXTERNAL" i) <- await
		flip sasl_ i . fromJust . lookup "EXTERNAL" . mkSaslServers
			. (: []) . RTExternal
			. rt' . fromJust $ lookup From as
	_ -> return ()
	where
	rt' d "" = rt d
	rt' _ hn = throwError $ fromSaslError NotAuthorized hn

sasl_ :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	(Bool, Pipe BS.ByteString (Either Success BS.ByteString) m ())
		-> Maybe BS.ByteString -> Pipe Xmpp Xmpp m ()
sasl_ r i = let (b, s) = r in saslPipe b i s

saslPipe :: (MonadState m, SaslState (StateType m)) => Bool
	-> Maybe BS.ByteString
	-> Pipe BS.ByteString (Either Success BS.ByteString) m ()
	-> Pipe Xmpp Xmpp m ()
saslPipe True (Just i) s =
	(yield i >> convert (\(SRResponse r) -> r)) =$= s =$= outputScram
saslPipe True _ s =
	convert (\(SRResponse r) -> r) =$= s =$= (yield (SRChallenge "") >> outputScram)
saslPipe False Nothing s = convert (\(SRResponse r) -> r) =$= s =$= outputScram
saslPipe _ _ _ = error "saslPipe: no need of initial data"

outputScram :: (MonadState m, SaslState (StateType m)) =>
	Pipe (Either Success BS.ByteString) Xmpp m ()
outputScram = await >>= \mch -> case mch of
	Just (Right r) -> yield (SRChallenge r) >> outputScram
	Just (Left (Success r)) -> yield $ XCSaslSuccess r
	Nothing -> return ()

begin :: (MonadState m, StateType m ~ XmppState) =>
	Pipe BS.ByteString BS.ByteString m [Xmlns]
begin = inputBegin =@= process  =$= outputS

process :: (MonadState m, StateType m ~ XmppState) => Pipe Xmpp Xmpp m ()
process = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		yield XCDecl
		nextUuid >>= yield . begin_
		yield $ XCFeatures []
		process
	_ -> return ()
