{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
	TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe
import Data.Pipe.Basic
import Data.HandleLike
import System.Environment
import System.IO.Unsafe
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import XmppClient
import Tools
import Caps
import Disco
import Delay

host :: BS.ByteString
host = case (\(Jid _ d _) -> d) sender of
	"otherhost" -> "localhost"
	h -> h

port :: PortID
port = PortNumber $ case (\(Jid _ d _) -> d) sender of
	"otherhost" -> 55222
	_ -> 5222

cipherSuites :: [CipherSuite]
cipherSuites = ["TLS_RSA_WITH_AES_128_CBC_SHA"]

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/xmpp_client.sample_key"
	c <- readCertificateChain ["certs/xmpp_client.sample_cert"]
	(g :: SystemRNG) <- cprgCreate <$> createEntropyPool
	h <- connectTo (BSC.unpack host) port
	void . runPipe $ input h =$=
		hlpDebug h =$= (begin host "en" >> starttls) =$= output h
	(`run` g) $ do
		p <- open' h (BSC.unpack host) cipherSuites [(k, c)] ca
		((), st) <- xmpp (SHandle p) `runStateT` St [
			("username", (\(Jid u _ _) -> u) sender),
			("authcid", (\(Jid u _ _) -> u) sender),
			("password", "password"),
			("cnonce", "00DEADBEEF00") ]
		liftIO $ print st
		voidM $ xmpp (SHandle p) `runStateT` St []

xmpp :: (HandleLike h,
		MonadState (HandleMonad h), St ~ StateType (HandleMonad h),
		MonadError (HandleMonad h), Error (ErrorType (HandleMonad h)) ) =>
	h -> HandleMonad h ()
xmpp h = voidM . runPipe $
	input h =$= convert readDelay =$= hlpDebug h =$= proc =$= output h

readDelay :: Xmpp ->
	Either Xmpp (BS.ByteString, BS.ByteString, Maybe Jid, Jid, DelayedMessage)
readDelay (XCMessage Chat i f t (MBodyRaw ns))
	| Just dm <- toDelayedMessage ns = Right ("CHAT", i, f, t, dm)
readDelay x = Left x

proc :: (Monad m,
	MonadState m, StateType m ~ St, MonadError m, Error (ErrorType m) ) =>
	Pipe (Either Xmpp (BS.ByteString, BS.ByteString,
		Maybe Jid, Jid, DelayedMessage)) Xmpp m ()
proc = (convert (\(Left x) -> x) =$= begin host "en") >> process

process :: (Monad m,
		MonadState m, StateType m ~ St,
		MonadError m, Error (ErrorType m) ) =>
	Pipe (Either Xmpp (BS.ByteString, BS.ByteString, Maybe Jid, Jid, DelayedMessage)) Xmpp m ()
process = await >>= \mr -> case mr of
	Just (Left (XCFeatures fts))
		| Just (FtMechanisms ms) <- find isFtMechanisms fts,
			Just n <- listToMaybe $ saslList `intersect` ms -> do
			convert (\(Left x) -> x) =$= sasl n
	Just (Left (XCFeatures _fs)) -> mapM_ yield binds >> process
	Just (Left (SRPresence _ ns)) -> case toCaps ns of
		C [(CTHash, "sha-1"), (CTVer, v), (CTNode, n)] ->
			yield (getCaps v n) >> process
		_ -> process
	Just (Left (SRIq Get i (Just f) (Just (Jid u d _)) (QueryRaw ns)))
		| Just (IqDiscoInfoNode [(DTNode, n)]) <- toQueryDisco ns,
			(u, d) == let Jid u' d' _ = sender in (u', d') -> do
			yield $ resultCaps i f n
			yield . XCMessage Chat "prof_3" Nothing recipient $
				MBody message
			yield XCEnd
	Just _ -> process
	_ -> return ()
	where
	saslList = ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]
	isFtMechanisms (FtMechanisms _) = True
	isFtMechanisms _ = False

binds :: [Xmpp]
binds = [
	SRIq Set "_xmpp_bind1" Nothing Nothing . IqBind Nothing $
		Resource "profanity",
	SRIq Set "_xmpp_session1" Nothing Nothing IqSession,
	SRIq Get "_xmpp_roster1" Nothing Nothing $ IqRoster Nothing,
	SRPresence [(Id, "prof_presence_1")] . fromCaps $
		capsToXmlCaps profanityCaps "http://www.profanity.im" ]

getCaps :: BS.ByteString -> BS.ByteString -> Xmpp
getCaps v n = SRIq Get "prof_caps_2" Nothing (Just sender) . QueryRaw .
	fromQueryDisco $ IqCapsQuery v n

resultCaps :: BS.ByteString -> Jid -> BS.ByteString -> Xmpp
resultCaps i t n = SRIq Result i Nothing (Just t) . QueryRaw . fromQueryDisco
	$ IqCapsQuery2 [capsToQuery profanityCaps n]

message :: BS.ByteString
sender, recipient :: Jid
(sender, recipient, message) = unsafePerformIO $ do
	[s, r, m] <- getArgs
	return (toJid $ BSC.pack s, toJid $ BSC.pack r, BSC.pack m)
