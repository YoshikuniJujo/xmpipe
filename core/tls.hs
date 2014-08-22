{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
	TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Data.Maybe
import Data.Pipe
import System.Environment
import System.IO.Unsafe
import Text.XML.Pipe
import Network
import Network.PeyoTLS.TChan.Client
-- import Network.PeyoTLS.Client
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
host = case (\(Jid _ d _) -> d) sender of "otherhost" -> "localhost"; h -> h

port :: PortID
port = PortNumber $
	case (\(Jid _ d _) -> d) sender of "otherhost" -> 55222; _ -> 5222

sender, recipient :: Jid; message :: BS.ByteString
(sender, recipient, message) = unsafePerformIO $ do
	[s, r, m] <- getArgs
	return (toJid $ BSC.pack s, toJid $ BSC.pack r, BSC.pack m)

cipherSuites :: [CipherSuite]
cipherSuites = ["TLS_RSA_WITH_AES_128_CBC_SHA"]

mechanisms :: [BS.ByteString]
mechanisms = ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]

debug :: (MonadIO m, Show a) => Pipe a a m ()
debug = await >>= maybe (return ()) (\x -> liftIO (print x) >> yield x >> debug)

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/xmpp_client.sample_key"
	c <- readCertificateChain ["certs/xmpp_client.sample_cert"]
	(g :: SystemRNG) <- cprgCreate <$> createEntropyPool
	h <- connectTo (BSC.unpack host) port
	starttls h host
-- {-
	(inc, otc) <- open' h (BSC.unpack host) cipherSuites [(k, c)] ca g
	dbgc <- atomically newTChan
	forkIO . forever $ do
		bs <- atomically $ readTChan dbgc
		print bs
		atomically $ writeTChan otc bs
--	let sh = SHandle h
	voidM . (`runStateT` saslInit) $ do
		saslC inc otc host mechanisms
		Just ns <- bindC inc otc host
		voidM . runPipe $ inputC inc ns
			=$= debug
			=$= (putPresence >> process)
			=$= outputC dbgc
-- -}
{-
	(`run` g) $ do
		p <- open' h (BSC.unpack host) cipherSuites [(k, c)] ca
		let sp = SHandle p
		voidM . (`runStateT` saslInit) $ do
			sasl sp host mechanisms
			Just ns <- bind sp host
			voidM . runPipe $ input sp ns =$= hlpDebug sp =$=
				(putPresence >> process) =$= output sp
-}
	threadDelay 1000000
	where
	saslInit = mkSaslInit ((\(Jid u _ _) -> u) sender) "password" "00DEADBEEF00"

putPresence ::
	(MonadState m, St ~ (StateType m), MonadError m, Error (ErrorType m)) =>
	Pipe a Xmpp m ()
putPresence = do
	gets stFeatures >>= mapM_ yield . mapMaybe resp
	yield . SRPresence tagsNull { tagId = Just "prof_presence_1" }
		. fromCaps
		$ capsToXmlCaps profanityCaps "http://www.profanity.im"
	where
	resp (FRRosterver _) = Just $ SRIq
		tagsGet { tagId = Just "_xmpp-roster1" }
		[fromIRRoster $ IRRoster Nothing]
	resp _ = Nothing

process :: (
	MonadState m, SaslState (StateType m),
	MonadError m, Error (ErrorType m) ) => Pipe Xmpp Xmpp m ()
process = await >>= \mr -> case mr of
	Just (SRPresence _ ns) -> case toCaps ns of
		C [(CTHash, "sha-1"), (CTVer, v), (CTNode, n)] -> do
{-
			yield $ SRMessage tagsChat {
					tagId = Just "prof_3",
					tagTo = Just recipient }
				[XmlNode (nullQ "body") [] [] [XmlCharData message]]
-}
			yield (getCaps v n) >> process
		_ -> process
	Just (SRIq Tags {
			tagType = Just "get", tagId = Just i,
			tagFrom = Just f, tagTo = Just (Jid u d _) } ns)
		| Just (IqDiscoInfoNode [(DTNode, n)]) <- toQueryDisco ns,
			(u, d) == let Jid u' d' _ = sender in (u', d') -> do

			yield $ resultCaps i f n

			yield $ SRMessage tagsChat {
					tagId = Just "prof_11",
					tagTo = Just recipient }
				[XmlNode (nullQ "body") [] [] [XmlCharData "hogeta"]]

			yield $ SRMessage tagsChat {
					tagId = Just "prof_3",
					tagTo = Just recipient }
				[XmlNode (nullQ "body") [] [] [XmlCharData message]]

			yield $ SRMessage tagsChat {
					tagId = Just "prof_15",
					tagTo = Just recipient }
				[XmlNode (nullQ "body") [] [] [XmlCharData "HOGETA"]]

			yield XCEnd
	Just _ -> process
	_ -> return ()

{-
toMessage :: BS.ByteString -> Xmpp
toMessage m = SRMessage tagsChat { tagId = Just "hoge", tagTo = Just recipient }
	[XmlNode (nullQ "body") [] [] [XmlCharData m]]
	-}

getCaps :: BS.ByteString -> BS.ByteString -> Xmpp
getCaps v n = SRIq tagsGet { tagId = Just "prof_caps_2", tagTo = Just sender }
	. fromQueryDisco $ IqCapsQuery v n

resultCaps :: BS.ByteString -> Jid -> BS.ByteString -> Xmpp
resultCaps i t n = SRIq tagsResult { tagId = Just i, tagTo = Just t }
	. fromQueryDisco $ IqCapsQuery2 [capsToQuery profanityCaps n]
