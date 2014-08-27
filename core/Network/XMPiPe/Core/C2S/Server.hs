{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts,
	PackageImports #-}

module Network.XMPiPe.Core.C2S.Server (
	-- * Types and Values
	Mpi(..), Feature, Jid(..), Tags(..), tagsType,
	XmppState(..), Retrieve(..),
	-- * Functions
	starttls, sasl, bind, input, output,
	) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Pipe
import Text.XML.Pipe

import Xmpp hiding (input, output, Feature)
import qualified Xmpp as X
import SaslServer

import qualified Data.ByteString as BS

type Feature = XmlNode

input :: Monad m => [Xmlns] -> Pipe BS.ByteString Mpi m ()
input = inputMpi

output :: Monad m => Pipe Mpi BS.ByteString m ()
output = outputMpi

starttls :: (MonadState m, [BS.ByteString] ~ StateType m) =>
	BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
starttls hst = inputP3 =$= processTls hst =$= X.output

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
sasl hst rt = inputP2 =$= makeSasl hst rt =$= X.output

makeSasl :: (
	MonadState m, XmppState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> [Retrieve m] -> Pipe Xmpp Xmpp m ()
makeSasl hst rt = await >>= \p -> case p of
	Just (XCBegin _) -> do
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
	BS.ByteString -> [Feature] -> Pipe BS.ByteString BS.ByteString m [Xmlns]
bind hst fts = inputP3 =@= makeBind hst fts =$= X.output

makeBind :: (
	MonadState m, XmppState (StateType m),
	MonadError m, SaslError (ErrorType m)) =>
	BS.ByteString -> [Feature] -> Pipe Xmpp Xmpp m ()
makeBind hst fts = await >>= \p -> case p of
	Just (XCBegin _) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, u),
			(From, hst),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		yield . XCFeatures . (FtBind Required :) $ map FtRaw fts
		makeBind hst fts
	Just (SRIqBind ts (IqBind (Just Required) (Resource n)))
		| Just "set" <- lookup Type ts, Just i <- lookup Id ts -> do
			lift . modify $ setResource n
			Just j <- lift $ fst `liftM` gets getXmppState
			yield . SRIqBind [(Type, "result"), (Id, i)]
				. IqBind Nothing $ BJid j
			makeBind hst fts
	_ -> return ()
