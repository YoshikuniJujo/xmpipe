{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
	TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
-- import Control.Concurrent.STM
import Data.Maybe
import Data.Pipe
import Data.HandleLike
import System.Environment
import System.IO.Unsafe
import Text.XML.Pipe
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import XmppClient
import Im
import Tools
import Caps
import Disco

host :: BS.ByteString
host = case (\(Jid _ d _) -> d) sender of
	"otherhost" -> "localhost"
	h -> h

port :: PortID
port = PortNumber $ case (\(Jid _ d _) -> d) sender of
	"otherhost" -> 55222
	_ -> 5222

message :: BS.ByteString
sender, recipient :: Jid
(sender, recipient, message) = unsafePerformIO $ do
	[s, r, m] <- getArgs
	return (toJid $ BSC.pack s, toJid $ BSC.pack r, BSC.pack m)

cipherSuites :: [CipherSuite]
cipherSuites = ["TLS_RSA_WITH_AES_128_CBC_SHA"]

mechanisms :: [BS.ByteString]
mechanisms = ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/xmpp_client.sample_key"
	c <- readCertificateChain ["certs/xmpp_client.sample_cert"]
	(g :: SystemRNG) <- cprgCreate <$> createEntropyPool
	h <- connectTo (BSC.unpack host) port
	starttls h host
	(`run` g) $ do
		p <- open' h (BSC.unpack host) cipherSuites [(k, c)] ca
		let sp = SHandle p
		((), _st) <- (`runStateT` saslInit) $ do
			sasl sp host mechanisms
			Just ns <- bind sp host
			voidM . runPipe $ input sp ns =$= hlpDebug sp =$=
				(putPresence >> process) =$= output sp
		return ()
--		liftIO $ print st

saslInit :: St
saslInit = St [] [
	("username", (\(Jid u _ _) -> u) sender),
	("authcid", (\(Jid u _ _) -> u) sender),
	("password", "password"),
	("cnonce", "00DEADBEEF00") ]

xmpp :: (HandleLike h, -- MonadIO (HandleMonad h),
	MonadState (HandleMonad h), St ~ (StateType (HandleMonad h)),
	MonadError (HandleMonad h), Error (ErrorType (HandleMonad h)) ) =>
	h -> [Xmlns] -> HandleMonad h ()
xmpp h ns = do
	voidM . runPipe $ input h ns =$= hlpDebug h =$=
		(putPresence >> process) =$= output h

putPresence :: (Monad m,
	MonadState m, St ~ (StateType m),
	MonadError m, Error (ErrorType m) ) => Pipe a Xmpp m ()
putPresence = do
	St fts _ <- get
	mapM_ yield $ mapMaybe responseToFeature fts
	yield . SRPresence tagsNull { tagId = Just "prof_presence_1" }
		. fromCaps
		$ capsToXmlCaps profanityCaps "http://www.profanity.im"

process :: (Monad m,
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) => Pipe Xmpp Xmpp m ()
process = await >>= \mr -> case mr of
	Just (SRPresence _ ns) -> case toCaps ns of
		C [(CTHash, "sha-1"), (CTVer, v), (CTNode, n)] ->
			yield (getCaps v n) >> process
		_ -> process
	Just (SRIq ts ns)
		| Just (IqDiscoInfoNode [(DTNode, n)]) <- toQueryDisco ns,
			Just "get" <- tagType ts,
			Just i <- tagId ts,
			Just f <- tagFrom ts,
			Just (Jid u d _) <- tagTo ts,
			(u, d) == let Jid u' d' _ = sender in (u', d') -> do
			yield $ resultCaps i f n
			yield $ SRMessage
				Tags {	tagId = Just "prof_3",
					tagType = Just "chat",
					tagFrom = Nothing,
					tagTo = Just recipient,
					tagLang = Nothing,
					tagOthers = [] }
				[XmlNode (nullQ "body") [] []
					[XmlCharData message]]
			yield XCEnd
	Just _ -> process
	_ -> return ()

getCaps :: BS.ByteString -> BS.ByteString -> Xmpp
getCaps v n = SRIq tagsGet { tagId = Just "prof_caps_2", tagTo = Just sender }
	. fromQueryDisco $ IqCapsQuery v n

resultCaps :: BS.ByteString -> Jid -> BS.ByteString -> Xmpp
resultCaps i t n = SRIq tagsResult { tagId = Just i, tagTo = Just t }
	. fromQueryDisco $ IqCapsQuery2 [capsToQuery profanityCaps n]

tagsResult :: Tags
tagsResult = Tags Nothing (Just "result") Nothing Nothing Nothing []
