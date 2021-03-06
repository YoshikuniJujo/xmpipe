{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}

import Data.UUID
import System.Environment
import System.Random

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe
import Network

import XmppServer

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
	pn : _ <- getArgs
	socket <- listenOn . PortNumber . fromIntegral $ (read :: String -> Int) pn
	forever $ do
		(h, _, _) <- accept socket
		uuids <- randoms <$> getStdGen
		voidM . forkIO . (`evalStateT` initXmppState uuids)
			. xmpp $ SHandle h

xmpp :: (MonadState (HandleMonad h), StateType (HandleMonad h) ~ XmppState,
		HandleLike h) => h -> HandleMonad h ()
xmpp h = do
	voidM . runPipe $ input h =$= makeP =$= output h
	hlPut h $ xmlString [XmlEnd (("stream", Nothing), "stream")]
	hlClose h

makeP :: (MonadState m, StateType m ~ XmppState) =>
	Pipe Common Common m ()
makeP = (,) `liftM` await `ap` lift (gets receiver) >>= \p -> case p of
	(Just (SRStream _), Nothing) -> do
		yield SRXmlDecl
		lift nextUuid >>= \u -> yield $ SRStream [
			(Id, toASCIIBytes u),
			(From, "localhost"), (Version, "1.0"), (Lang, "en") ]
		lift nextUuid >>= digestMd5 Nothing >>= \un -> lift . modify .
			setReceiver $ Jid un "localhost" Nothing
		makeP
	(Just (SRStream _), _) -> do
		yield SRXmlDecl
		lift nextUuid >>= \u -> yield $ SRStream [
			(Id, toASCIIBytes u),
			(From, "localhost"), (Version, "1.0"), (Lang, "en") ]
		yield $ SRFeatures
			[Rosterver Optional, Bind Required, Session Optional]
		makeP
	(Just (SRIq Set i Nothing Nothing
		(IqBind (Just Required) (Resource n))), _) -> do
		lift $ modify (setResource n)
		Just j <- lift $ gets receiver
		yield . SRIq Result i Nothing Nothing
			. IqBind Nothing $ BJid j
		makeP
	(Just (SRIq Set i Nothing Nothing IqSession), mrcv) -> do
		yield (SRIq Result i Nothing mrcv IqSessionNull)
		makeP
	(Just (SRIq Get i Nothing Nothing (IqRoster Nothing)), mrcv) -> do
		yield . SRIq Result i Nothing mrcv
			. IqRoster . Just $ Roster (Just "1") []
		makeP
	(Just (SRPresence _ _), Just rcv) ->
		yield (SRMessage Chat "hoge" (Just sender) rcv .
			MBody $ MessageBody "Hi!") >> makeP
	_ -> return ()

voidM :: Monad m => m a -> m ()
voidM = (>> return ())

sender :: Jid
sender = Jid "yoshio" "localhost" (Just "profanity")
