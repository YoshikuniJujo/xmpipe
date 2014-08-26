{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XMPiPe.Core.C2S.Server (
	-- * Types and Values
	Mpi(..), Jid(..), Tags(..), tagsType,
	XmppState(..), Retrieve(..),
	-- * Functions
	starttls, sasl, bind, inputMpi, outputMpi,
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Text.XML.Pipe

import Xmpp
import Im
import SaslServer

import qualified Data.ByteString as BS

starttls :: (MonadState m, [BS.ByteString] ~ StateType m) =>
	BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
starttls hst = inputP3 =$= processTls hst =$= output

processTls :: (MonadState m, [BS.ByteString] ~ StateType m) =>
	BS.ByteString -> Pipe Xmpp Xmpp m ()
processTls hst = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		u : us <- get
		put us
		yield XCDecl
		yield $ XCBegin [
			(Id, u), (From, hst),
			(TagRaw $ nullQ "version", "1.0"), (Lang, "en") ]
		yield $ XCFeatures [FtStarttls Required]
		processTls hst
	Just XCStarttls -> yield XCProceed
	Just _ -> error "processTls: bad"
	_ -> return ()

sasl :: (
	MonadState m, XmppState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> [Retrieve m] -> Pipe BS.ByteString BS.ByteString m ()
sasl hst rt = inputP2 =$= makeSasl hst rt =$= output

makeSasl :: (
	MonadState m, XmppState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> [Retrieve m] -> Pipe Xmpp Xmpp m ()
makeSasl hst rt = (,) `liftM` await `ap` lift (fst `liftM` gets getXmppState) >>= \p -> case p of
	(Just (XCBegin _), Nothing) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, u), (From, hst),
			(TagRaw $ nullQ "version", "1.0"), (Lang, "en") ]
		runSasl rt
	_ -> return ()

class SaslState xs => XmppState xs where
	getXmppState :: xs -> (Maybe Jid, [BS.ByteString])
	putXmppState :: (Maybe Jid, [BS.ByteString]) -> xs -> xs

nextUuid :: (MonadState m, XmppState (StateType m)) => m BS.ByteString
nextUuid = do
	xs <- get
	let (r, u : us) = getXmppState xs
	modify $ putXmppState (r, us)
	return u

setResource :: XmppState xs => BS.ByteString -> xs -> xs
setResource r xs
	| (Just (Jid a d _), ul) <- getXmppState xs =
		putXmppState (Just . Jid a d $ Just r, ul) xs
setResource _ _ = error "setResource: can't set resource to Nothing"

bind :: (
	MonadState m, XmppState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> Pipe BS.ByteString BS.ByteString m [Xmlns]
bind hst = inputP3 =@= makeBind hst =$= output

makeBind :: (
	MonadState m, XmppState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> Pipe Xmpp Xmpp m ()
makeBind hst = (,) `liftM` await `ap` lift (fst `liftM` gets getXmppState) >>= \p -> case p of
	(Just (XCBegin _), _) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, u),
			(From, hst),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		yield . XCFeatures $ map featureRToFeature
			[FRRosterver Optional, Ft $ FtBind Required]
		makeBind hst
	(Just (SRIqBind ts (IqBind (Just Required) (Resource n))), _)
		| Just "set" <- lookup Type ts,
			Just i <- lookup Id ts -> do
			lift $ modify (setResource n)
			Just j <- lift $ fst `liftM` gets getXmppState
			yield . SRIqBind [(Type, "result"), (Id, i)]
				. IqBind Nothing $ BJid j
			makeBind hst
	_ -> return ()
