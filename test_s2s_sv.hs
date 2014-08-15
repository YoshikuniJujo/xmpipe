{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.HandleLike
import Data.UUID
import System.Random
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

import TestFederation
import Digest

data SHandle s h = SHandle h

instance HandleLike h => HandleLike (SHandle s h) where
	type HandleMonad (SHandle s h) = StateT s (HandleMonad h)
	type DebugLevel (SHandle s h) = DebugLevel h
	hlPut (SHandle h) = lift . hlPut h
	hlGet (SHandle h) = lift . hlGet h
	hlClose (SHandle h) = lift $ hlClose h
	hlDebug (SHandle h) = (lift .) . hlDebug h

instance SaslError Alert where
	fromSaslError et em = ExternalAlert $ show et ++ ":" ++ show em

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/otherhost.sample_key"
	c <- readCertificateChain ["certs/otherhost.sample_cert"]
	soc <- listenOn $ PortNumber 55269
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		let sh = SHandle h
		sg <- newStdGen
		let us = randoms sg :: [UUID]
		void . forkIO . (`evalStateT` g0) $ do
			us' <- lift . (`execStateT` XmppState False us) . runPipe $
				input sh =$= processTls =$= output sh
			g <- StateT $ return . cprgFork
			liftIO . (`run` g) $ do
				p <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] (Just ca)
				getNames p >>= liftIO . print
				let sp = SHandle p
				void . (`evalStateT` us')
					. runPipe
					$ input sp =$= process =$= output sp
--				hlClose p

data XmppState = XmppState {
	xsAuthed :: Bool,
	xsUuid :: [UUID] }
	deriving Show

instance SaslState XmppState where
	getSaslState _ = [("username", "")]
	putSaslState _ = id

authed :: XmppState -> XmppState
authed xs = xs { xsAuthed = True }

dropUuid :: XmppState -> XmppState
dropUuid xs = xs { xsUuid = tail $ xsUuid xs }

retrieve :: (MonadError m, SaslError (ErrorType m)) => BS.ByteString -> m ()
retrieve "" = return ()
retrieve hn = throwError $ fromSaslError NotAuthorized hn

process :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m) ) => Pipe Common Common m ()
process = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		a <- lift $ gets xsAuthed
		yield XCDecl
		nextUuid >>= yield . begin
		yield . XCFeatures $ if a then [] else [FtMechanisms ["EXTERNAL"]]
		process
	Just (XCAuth "EXTERNAL" i) -> do
		lift $ modify authed
		sasl (RTExternal retrieve) i
--		yield $ XCSaslSuccess Nothing
		process
	Just XCMessage{} -> do
		yield . XCMessage Chat "hoge"
			(Just $ Jid "yoshio" "otherhost" Nothing)
			(Jid "yoshikuni" "localhost" Nothing) $
			MBodyRaw [XmlCharData "HOGETA"]
		process
--		yield XEnd
--		process
	_ -> return ()

processTls :: (MonadState m, StateType m ~ XmppState) => Pipe Common Common m ()
processTls = await >>= \mx -> case mx of
	Just (XCBegin _as) -> do
		yield XCDecl
		nextUuid >>= yield . begin
		yield $ XCFeatures [FtStarttls Required]
		processTls
	Just XCStarttls -> yield XCProceed
	_ -> return ()

begin :: UUID -> Common
begin u = XCBegin [
	(From, "otherhost"),
	(To, "localhost"),
	(Version, "1.0"),
	(Id, toASCIIBytes u)
	]

nextUuid :: (MonadState m, StateType m ~ XmppState) => Pipe a b m UUID
nextUuid = lift $ do
	u <- gets $ head . xsUuid
	modify dropUuid
	return u

sasl :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	Retrieve m -> Maybe BS.ByteString -> Pipe Common Common m ()
sasl r i = let (_, (b, s)) = saslServer r in saslPipe b i s

saslPipe :: (MonadState m, SaslState (StateType m)) => Bool
	-> Maybe BS.ByteString
	-> Pipe BS.ByteString (Either Success BS.ByteString) m ()
	-> Pipe Common Common m ()
saslPipe True (Just i) s =
	(yield i >> convert (\(SRResponse r) -> r)) =$= s =$= outputScram
saslPipe True _ s =
	convert (\(SRResponse r) -> r) =$= s =$= (yield (SRChallenge "") >> outputScram)
saslPipe False Nothing s = convert (\(SRResponse r) -> r) =$= s =$= outputScram
saslPipe _ _ _ = error "saslPipe: no need of initial data"

outputScram :: (MonadState m, SaslState (StateType m)) =>
	Pipe (Either Success BS.ByteString) Common m ()
outputScram = await >>= \mch -> case mch of
	Just (Right r) -> yield (SRChallenge r) >> outputScram
	Just (Left (Digest.Success r)) -> yield $ XCSaslSuccess r
	Nothing -> return ()

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = await >>= maybe (return ()) (\x -> yield (f x) >> convert f)
