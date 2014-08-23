{-# LANGUAGE OverloadedStrings, TypeFamilies, TupleSections, FlexibleContexts,
	PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent.STM

import Data.UUID
import System.Environment
import System.Random

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent (forkIO)
import Data.Pipe
import Data.HandleLike
import Data.X509
import Data.X509.CertificateStore
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

import XmppServer
import Im
import FederationClientIm

instance SaslError Alert where
	fromSaslError et em = ExternalAlert $ show et ++ ":" ++ show em

main :: IO ()
main = do
	sl <- atomically $ newTVar []
--	ca <- readCertificateStore ["certs/cacert.sample_pem"]
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
			voidM . liftIO . runPipe $ fromHandleLike h =$= inputP3
				=$= hlpDebug h
				=$= processTls
				=$= output
				=$= toHandleLike h
			liftIO . (`run` g) $ do
				p <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"]
					[(k, c)] Nothing
--					[(k, c)] (Just ca)
				getNames p >>= liftIO . print
				(`evalStateT` initXmppState uuids) $ do
					let sp = SHandle p
					voidM . runPipe $ fromHandleLike sp
						=$= inputP2
						=$= hlpDebug sp
						=$= makeP
						=$= output
						=$= toHandleLike sp
					voidM . runPipe $ fromHandleLike sp
						=$= inputP2
						=$= hlpDebug sp
						=$= makeP
						=$= outputSel sl sp
					hlPut sp $ xmlString [XmlEnd
						(("stream", Nothing), "stream")]
					hlClose sp

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

processTls :: (
	MonadError m, SaslError (ErrorType m)) => Pipe Xmpp Xmpp m ()
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

outputSel :: (MonadIO (HandleMonad h),
	MonadState (HandleMonad h), StateType (HandleMonad h) ~ XmppState,
	HandleLike h) => TVar [(String, TChan Xmpp)]
		-> h -> Pipe Xmpp () (HandleMonad h) ()
outputSel sl h = await >>= \mx -> case mx of
	Just m@(SRMessage as _b)
		| Just (Jid "yoshio" "otherhost" Nothing) <- tagTo as -> do
			l <- liftIO (atomically $ readTVar sl)
			maybe (otherhost sl m)
				(liftIO . atomically . flip writeTChan m)
				$ lookup "otherhost" l
			outputSel sl h
	Just x -> lift (hlPut h $ xmlString [fromCommon Client x]) >> outputSel sl h
	_ -> return ()

otherhost :: MonadIO m =>
	TVar [(String, TChan Xmpp)] -> Xmpp -> Pipe Xmpp () m ()
otherhost sl m = liftIO $ do
	(ca, k, c) <- readFiles
	(ip, e) <- connect ca k c
	atomically $ writeTChan ip m
	atomically $ readTChan e
	atomically $ modifyTVar sl (("otherhost", ip) :)

readFiles :: IO (CertificateStore, CertSecretKey, CertificateChain)
readFiles = (,,)
	<$> readCertificateStore ["certs/cacert.sample_pem"]
	<*> readKey "certs/localhost.sample_key"
	<*> readCertificateChain ["certs/localhost.sample_crt"]

tagsChat :: Tags
tagsChat = Tags {
	tagId = Nothing,
	tagType = Just "chat",
	tagFrom = Nothing,
	tagTo = Nothing,
	tagLang = Nothing,
	tagOthers = [] }

tagsResult :: Tags
tagsResult = Tags Nothing (Just "result") Nothing Nothing Nothing []

makeP :: (
	MonadState m, StateType m ~ XmppState,
	MonadError m, SaslError (ErrorType m)) => Pipe Xmpp Xmpp m ()
makeP = (,) `liftM` await `ap` lift (gets receiver) >>= \p -> case p of
	(Just (XCBegin _), Nothing) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, toASCIIBytes u),
			(From, "localhost"),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		runSasl
	(Just (XCBegin _), _) -> do
		yield XCDecl
		lift nextUuid >>= \u -> yield $ XCBegin [
			(Id, toASCIIBytes u),
			(From, "localhost"),
			(TagRaw $ nullQ "version", "1.0"),
			(Lang, "en") ]
		yield . XCFeatures $ map featureRToFeature
			[FRRosterver Optional, Ft $ FtBind Required]
		makeP
	(Just (SRMessage _ bd), Just rcv) -> do
		let	ts1 = tagsChat { tagFrom = Just sender, tagTo = Just rcv }
			ts2 = tagsChat {
				tagFrom = Just $ Jid "yoshio" "otherhost" Nothing,
				tagTo = Just rcv }
			ts3 = tagsChat {
				tagFrom = Just $ Jid "yoshikuni" "localhost" $
					Just "profanity",
				tagTo = Just $ Jid "yoshio" "otherhost" Nothing }
		yield $ SRMessage ts1
			[XmlNode (nullQ "body") [] [] [XmlCharData "Hi, TLS!"]]
		yield $ SRMessage ts2 bd
		yield $ SRMessage ts3 bd
		makeP
	(Just (SRIq ts ns), _)
		| Just (IRRoster Nothing) <- toIRRoster ns,
			Just "get" <- tagType ts,
			Just i <- tagId ts -> do
			yield $ SRIq tagsResult { tagId = Just i }
				[fromIRRoster . IRRoster . Just
					$ Roster (Just "1") []]
			makeP
	(Just (SRIqBind ts (IqBind (Just Required) (Resource n))), _)
		| Just "set" <- lookup Type ts,
			Just i <- lookup Id ts -> do
			lift $ modify (setResource n)
			Just j <- lift $ gets receiver
			yield . SRIqBind [(Type, "result"), (Id, i)]
				. IqBind Nothing $ BJid j
			makeP
	(Just (SRPresence _ _), Just rcv) -> do
		let	ts1 = Tags {
				tagId = Nothing,
				tagType = Just "chat",
				tagFrom = Just sender,
				tagTo = Just rcv,
				tagLang = Nothing,
				tagOthers = [] }
		yield $ SRMessage ts1
			[XmlNode (nullQ "body") [] [] [XmlCharData "Hi, TLS!"]]
		makeP
	_ -> return ()

sender :: Jid
sender = Jid "yoshio" "otherhost" (Just "profanity")

setResource :: BS.ByteString -> XmppState -> XmppState
setResource r xs@XmppState{ receiver = Just (Jid a d _) } =
	xs { receiver = Just . Jid a d $ Just r }
setResource _ _ = error "setResource: can't set resource to Nothing"

nextUuid :: (MonadState m, StateType m ~ XmppState) => m UUID
nextUuid = do
	xs@XmppState { uuidList = u : us } <- get
	put xs { uuidList = us }
	return u

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
