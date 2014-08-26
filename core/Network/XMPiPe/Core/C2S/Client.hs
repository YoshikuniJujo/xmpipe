{-# LANGUAGE OverloadedStrings, TypeFamilies,
	FlexibleContexts, PackageImports #-}

module Network.XMPiPe.Core.C2S.Client (
	-- * Types and Values
	Mpi(..), Feature, Jid(..), toJid,
	Tags(..), tagsNull, tagsType,
	-- * Functions
	starttls, sasl, bind, input, output,
	) where

import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Writer
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

import Xmpp hiding (input, output, Feature)
import qualified Xmpp as X
import qualified SaslClient as SASL

input :: Monad m => [Xmlns] -> Pipe BS.ByteString Mpi m ()
input = inputMpi

output :: Monad m => Pipe Mpi BS.ByteString m ()
output = outputMpi

begin :: Monad m => BS.ByteString -> BS.ByteString -> Pipe Xmpp Xmpp m ()
begin h l = do
	yield XCDecl
	yield $ XCBegin [(To, h), (TagRaw $ nullQ "version", "1.0"), (Lang, l)]

starttls :: Monad m => BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
starttls hst = inputP3 =$= (begin hst "en" >> starttls_) =$= X.output

starttls_ :: Monad m => Pipe Xmpp Xmpp m ()
starttls_ = do
	Just (XCBegin _as) <- await
	Just (XCFeatures fs) <- await
	unless (any isSt fs) $ fail "starttls_: not support tls"
	yield XCStarttls
	Just XCProceed <- await
	return ()
	where isSt (FtStarttls _) = True; isSt _ = False

sasl :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m)) =>
	BS.ByteString -> [BS.ByteString] -> Pipe BS.ByteString BS.ByteString m ()
sasl hst ms = inputP2 =$= sasl' hst ms =$= X.output

sasl' :: (
	MonadState m, SASL.SaslState (StateType m),
	MonadError m, Error (ErrorType m)) =>
	BS.ByteString -> [BS.ByteString] -> Pipe Xmpp Xmpp m ()
sasl' hst ms = begin hst "en" >> sasl_ ms

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

bind :: (Monad m,
	MonadWriter m, [Feature] ~ WriterType m,
	MonadError m, Error (ErrorType m) ) =>
	BS.ByteString -> Pipe BS.ByteString BS.ByteString m [Xmlns]
bind hst = inputP3 =@= (begin hst "en" >> bind_) =$= X.output

bind_ :: (
	MonadWriter m, [Feature] ~ (WriterType m),
	MonadError m, Error (ErrorType m) ) => Pipe Xmpp Xmpp m ()
bind_ = await >>= \mr -> case mr of
	Just (XCFeatures fs) -> do
		let (b, fs') = sepBind fs
		mapM_ yield . catMaybes $
			map responseToFeature $ filter notFtSession b
		tell $ map getFeature fs'
		bind_
	Just _ -> bind_
	_ -> return ()

type Feature = XmlNode

getFeature :: X.Feature -> Feature
getFeature (FtRaw ft) = ft
getFeature _ = error "Network.XMPiPe.Core.C2S.Client.getFeature: bad"

sepBind :: [X.Feature] -> ([X.Feature], [X.Feature])
sepBind = partition notFtRaw

notFtSession :: X.Feature -> Bool
notFtSession (FtSession _) = False
notFtSession _ = True

notFtRaw :: X.Feature -> Bool
notFtRaw (FtRaw _) = False
notFtRaw _ = True

responseToFeature :: X.Feature -> Maybe Xmpp
responseToFeature (FtBind _) = Just
	. SRIqBind [(Type, "set"), (Id, "_xmpp_bind1")] . IqBind Nothing
	$ Resource "profanity"
responseToFeature _ = Nothing
