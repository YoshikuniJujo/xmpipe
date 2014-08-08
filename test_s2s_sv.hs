{-# LANGUAGE OverloadedStrings, TypeFamilies, PackageImports #-}

-- import Debug.Trace

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
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

import TestFederation

data SHandle s h = SHandle h

instance HandleLike h => HandleLike (SHandle s h) where
	type HandleMonad (SHandle s h) = StateT s (HandleMonad h)
	type DebugLevel (SHandle s h) = DebugLevel h
	hlPut (SHandle h) = lift . hlPut h
	hlGet (SHandle h) = lift . hlGet h
	hlClose (SHandle h) = lift $ hlClose h
	hlDebug (SHandle h) = (lift .) . hlDebug h

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

authed :: XmppState -> XmppState
authed xs = xs { xsAuthed = True }

dropUuid :: XmppState -> XmppState
dropUuid xs = xs { xsUuid = tail $ xsUuid xs }

process :: (MonadState m, StateType m ~ XmppState) => Pipe Common Common m ()
process = await >>= \mx -> case mx of
	Just (CCommon (XCBegin _as)) -> do
--		trace "HERE" $ return ()
		a <- lift $ gets xsAuthed
		yield $ CCommon XCDecl
		nextUuid >>= yield . begin
		yield $ if a
			then CCommon $ XCFeatures []
			else CCommon $ XCFeatures [FtMechanisms [External]]
--		trace "THERE" $ return ()
		process
	Just (CCommon (XCAuth External)) -> do
		lift $ modify authed
		yield $ CCommon XCSaslSuccess
		process
	Just (CCommon (XCMessage _ _ _ _ _)) -> do
		yield . CCommon $ XCMessage Chat "hoge"
			(Just $ Jid "yoshio" "otherhost" Nothing)
			(Jid "yoshikuni" "localhost" Nothing) $
			MBodyRaw [XmlCharData "HOGETA"]
		process
--		yield XEnd
--		process
	_ -> return ()

processTls :: (MonadState m, StateType m ~ XmppState) => Pipe Common Common m ()
processTls = await >>= \mx -> case mx of
	Just (CCommon (XCBegin _as)) -> do
		yield $ CCommon XCDecl
		nextUuid >>= yield . begin
		yield . CCommon $ XCFeatures [FtStarttls Required]
		processTls
	Just (CCommon XCStarttls) -> do
		yield $ CCommon XCProceed
	_ -> return ()

begin :: UUID -> Common
begin u = CCommon $ XCBegin [
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
