{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}

import Control.Concurrent.STM
import TestFederationCl

import Data.UUID
import System.Environment
import System.Random

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent (forkIO)
import Data.Maybe
import Data.Pipe
import Data.Pipe.List
import Data.HandleLike
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile

import "crypto-random" Crypto.Random

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
	sl <- atomically $ newTVar []
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/localhost.sample_key"
	c <- readCertificateChain ["certs/localhost.sample_crt"]
	pn : _ <- getArgs
	soc <- listenOn . PortNumber . fromIntegral $ (read :: String -> Int) pn
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	forever $ do
		(h, _, _) <- accept soc
		voidM . forkIO . (`evalStateT` g0) $ do
			uuids <- randoms <$> lift getStdGen
			g <- StateT $ return . cprgFork
			liftIO . hlPut h . xmlString $ begin ++ tlsFeatures
			voidM . liftIO . runPipe $ handleP h
				=$= xmlEvent
				=$= convert fromJust
				=$= (xmlBegin >>= xmlNodeUntil isStarttls)
				=$= checkP h
				=$= toList
			liftIO . hlPut h $ xmlString proceed
			liftIO . (`run` g) $ do
				p <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] Nothing
--					[(k, c)] (Just ca)
				getNames p >>= liftIO . print
				(`evalStateT` initXmppState uuids) .
					xmpp sl $ SHandle p

xmpp :: (MonadIO (HandleMonad h),
	MonadState (HandleMonad h), StateType (HandleMonad h) ~ XmppState,
		HandleLike h) =>
	TVar [(String, TChan Xmpp)] -> h -> HandleMonad h ()
xmpp sl h = do
	voidM . runPipe $ input h =$= makeP =$= output sl h
	hlPut h $ xmlString [XmlEnd (("stream", Nothing), "stream")]
	hlClose h

makeP :: (MonadState m, StateType m ~ XmppState) =>
	Pipe Common Common m ()
makeP = (,) `liftM` await `ap` lift (gets receiver) >>= \p -> case p of
	(Just (SRStream _), Nothing) -> do
		yield $ CCommon XCDecl
		lift nextUuid >>= \u -> yield $ SRStream [
			(Id, toASCIIBytes u),
			(From, "localhost"), (Version, "1.0"), (Lang, "en") ]
		lift nextUuid >>= digestMd5 Nothing >>= \un -> lift . modify .
			setReceiver $ Jid un "localhost" Nothing
		makeP
	(Just (SRStream _), _) -> do
		yield $ CCommon XCDecl
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
	(Just (SRIq Set i Nothing Nothing IqSession), mrcv) ->
		yield (SRIq Result i Nothing mrcv IqSessionNull) >> makeP
	(Just (SRIq Get i Nothing Nothing (IqRoster Nothing)), mrcv) -> do
		yield . SRIq Result i Nothing mrcv
			. IqRoster . Just $ Roster (Just "1") []
		makeP
	(Just (SRPresence _ _), Just rcv) ->
		yield (SRMessage Chat "hoge" (Just sender) rcv .
			MBody $ MessageBody "Hi, TLS!") >> makeP
	(Just (SRMessage Chat i fr to bd), Just rcv) -> do
		yield (SRMessage Chat "hoge" (Just sender) rcv .
			MBody $ MessageBody "Hi, TLS!")
		yield $ SRMessage Chat i
			(Just $ Jid "yoshio" "otherhost" Nothing) rcv bd
		yield $ SRMessage Chat i
			(Just . Jid "yoshikuni" "localhost" $ Just "profanity")
			to bd
		makeP
	_ -> return ()

voidM :: Monad m => m a -> m ()
voidM = (>> return ())

sender :: Jid
sender = Jid "yoshio" "otherhost" (Just "profanity")

isStarttls :: XmlNode -> Bool
isStarttls (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "starttls")
	_ [] []) = True
isStarttls _ = False

begin :: [XmlNode]
begin = [
	XmlDecl (1, 0),
	XmlStart (("stream", Nothing), "stream")
		[	("", "jabber:client"),
			("stream", "http://etherx.jabber.org/streams") ]
		[	(nullQ "id", "83e074ac-c014-432e9f21-d06e73f5777e"),
			(nullQ "from", "localhost"),
			(nullQ "version", "1.0"),
			((("xml", Nothing), "lang"), "en") ]
	]

tlsFeatures :: [XmlNode]
tlsFeatures =
	[XmlNode (("stream", Nothing), "features") [] [] [mechanisms, starttls]]

mechanisms :: XmlNode
mechanisms = XmlNode (nullQ "mechanisms")
	[("", "urn:ietf:params:xml:ns:xmpp-sasl")] []
	[	XmlNode (nullQ "mechanism") [] [] [XmlCharData "SCRAM-SHA-1"],
	 	XmlNode (nullQ "mechanism") [] [] [XmlCharData "DIGEST-MD5"] ]

starttls :: XmlNode
starttls = XmlNode (nullQ "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []

proceed :: [XmlNode]
proceed = (: []) $ XmlNode (nullQ "proceed")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []
