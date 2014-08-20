{-# LANGUAGE OverloadedStrings, TypeFamilies,
	FlexibleContexts, PackageImports #-}

module XmppClient (
	Xmpp(..), Jid(..), toJid, SASL.SaslState(..),
	Tags(..), tagsNull, tagsResult, tagsChat, tagsGet,
	starttls, sasl, bind, input, output, mkSaslInit,
	) where

import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.HandleLike
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

import Xmpp hiding (input)
import qualified Xmpp
import Im
import qualified SaslClient as SASL

input :: HandleLike h => h -> [Xmlns] -> Pipe () Xmpp (HandleMonad h) ()
input = input''

begin :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
begin h l = do
	yield XCDecl
	yield $ XCBegin [(To, h), (TagRaw $ nullQ "version", "1.0"), (Lang, l)]

starttls :: HandleLike h => h -> BS.ByteString -> HandleMonad h ()
starttls h hst = voidM . runPipe $ Xmpp.input h
--	=$= hlpDebug h
	=$= (begin hst "en" >> starttls_) =$= output h

starttls_ :: Monad m => Pipe Xmpp Xmpp m ()
starttls_ = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	when (null $ filter isSt fs) $ fail "starttls_: not support tls"
	yield XCStarttls
	Just XCProceed <- await
	return ()
	where isSt (FtStarttls _) = True; isSt _ = False

sasl :: (
	HandleLike h,
	MonadState (HandleMonad h), SASL.SaslState (StateType (HandleMonad h)),
	MonadError (HandleMonad h), Error (ErrorType (HandleMonad h)) ) =>
	h -> BS.ByteString -> [BS.ByteString] -> HandleMonad h ()
sasl h hst ms = voidM . runPipe $ Xmpp.input h
--	=$= hlpDebug h
	=$= (begin hst "en" >> sasl_ ms) =$= output h

sasl_ :: (Monad m, MonadState m, SASL.SaslState (StateType m), MonadError m, Error (ErrorType m) ) =>
	[BS.ByteString] -> Pipe Xmpp Xmpp m ()
sasl_ sl = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	let	Just (FtMechanisms ms) = find isFtMechanisms fs
		Just n = listToMaybe $ sl `intersect` ms
	SASL.sasl n
	where
	isFtMechanisms (FtMechanisms _) = True
	isFtMechanisms _ = False

bind :: (
	HandleLike h,
	MonadState (HandleMonad h), St ~ (StateType (HandleMonad h)),
	MonadError (HandleMonad h), Error (ErrorType (HandleMonad h)) ) =>
	h -> BS.ByteString -> HandleMonad h (Maybe [Xmlns])
bind h hst = runPipe $ input' h =@=
--	hlpDebug h =$=
	(begin hst "en" >> bind_) =$= output h

bind_ :: (
	MonadState m, St ~ (StateType m),
	MonadError m, Error (ErrorType m) ) => Pipe Xmpp Xmpp m ()
bind_ = await >>= \mr -> case mr of
	Just (XCFeatures fs) -> do
		let (b, fs') = sepBind $ map featureToFeatureR fs
		lift . modify $ putFeatures fs'
		mapM_ yield . catMaybes $ map responseToFeature b
		modify $ putFeatures fs'
		bind_
	Just _ -> bind_
	_ -> return ()

sepBind :: [FeatureR] -> ([FeatureR], [FeatureR])
sepBind = partition isFtBind

isFtBind :: FeatureR -> Bool
isFtBind (Ft (FtBind _)) = True
isFtBind _ = False

responseToFeature :: FeatureR -> Maybe Xmpp
responseToFeature (Ft (FtBind _)) = Just
	. SRIqBind [(Type, "set"), (Id, "_xmpp_bind1")] . IqBind Nothing
	$ Resource "profanity"
-- responseToFeature (FRRosterver _) = Just $ SRIq
--	tagsGet { tagId = Just "_xmpp-roster1" } [fromIRRoster $ IRRoster Nothing]
responseToFeature _ = Nothing

tagsGet :: Tags
tagsGet = Tags Nothing (Just "get") Nothing Nothing Nothing []

putFeatures :: [FeatureR] -> St -> St
putFeatures fts (St _ ss) = St fts ss

tagsChat :: Tags
tagsChat = Tags Nothing (Just "chat") Nothing Nothing Nothing []

tagsResult :: Tags
tagsResult = Tags Nothing (Just "result") Nothing Nothing Nothing []

mkSaslInit :: BS.ByteString -> BS.ByteString -> BS.ByteString -> St
mkSaslInit un pw cn = St []
	[ ("username", un), ("authcid", un), ("password", pw), ("cnonce", cn) ]
