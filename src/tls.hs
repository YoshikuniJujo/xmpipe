{-# LANGUAGE OverloadedStrings, TypeFamilies, PackageImports, FlexibleContexts #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Pipe
import Data.HandleLike
import System.Environment
import System.IO.Unsafe
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import XmppClient
import Caps

host :: String
host = case BSC.unpack $ jidToHost sender of
	"otherhost" -> "localhost"
	h -> h

port :: PortID
port = PortNumber $ case jidToHost sender of
	"otherhost" -> 55222
	_ -> 5222

main :: IO ()
main = do
	h <- connectTo host port
	void . runPipe $ (yield begin >> yield startTls) =$= output h
	void . runPipe $ handleP h
		=$= xmlEvent
		=$= convert fromJust
		=$= (xmlBegin >>= xmlNodeUntil isProceed)
		=$= awaitAll
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/xmpp_client.sample_key"
	c <- readCertificateChain ["certs/xmpp_client.sample_cert"]
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`run` g) $ do
		p <- open' h "localhost" ["TLS_RSA_WITH_AES_128_CBC_SHA"]
			[(k, c)] ca
		((), st) <- xmpp (SHandle p) `runStateT` XmppState [
				("username", jidToUser sender),
				("authcid", jidToUser sender),
				("password", "password"),
				("rspauth", ""),
				("cnonce", "00DEADBEEF00")
				]
		liftIO $ print st

pipe :: Monad m => Pipe a a m ()
pipe = await >>= maybe (return ()) yield

awaitAll :: Monad m => Pipe a () m ()
awaitAll = await >>= maybe (return ()) (const awaitAll)

startTls :: Common
startTls = XCRaw $ XmlNode (("", Nothing), "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []

xmpp :: (HandleLike h,
		MonadState (HandleMonad h), XmppState ~ StateType (HandleMonad h),
		MonadError (HandleMonad h), Error (ErrorType (HandleMonad h)) ) =>
	h -> HandleMonad h ()
xmpp h = voidM . runPipe $ input h =$= proc =$= output h

proc :: (Monad m,
		MonadState m, StateType m ~ XmppState,
		MonadError m, Error (ErrorType m) ) =>
	Pipe Common Common m ()
proc = yield XCDecl
	>> yield (XCBegin [(To, "localhost"), (Version, "1.0"), (Lang, "en")])
	>> process

jidToHost :: Jid -> BS.ByteString
jidToHost (Jid _ d _) = d

jidToUser :: Jid -> BS.ByteString
jidToUser (Jid u _ _) = u

saslList :: [BS.ByteString]
-- saslList = ["PLAIN", "SCRAM-SHA-1", "DIGEST-MD5"]
saslList = ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]

getMatched :: [BS.ByteString] -> [BS.ByteString] -> Maybe BS.ByteString
getMatched xs ys = listToMaybe $ xs `intersect` ys

isFtMechanisms :: Feature -> Bool
isFtMechanisms (FtMechanisms _) = True
isFtMechanisms _ = False

process :: (Monad m,
		MonadState m, StateType m ~ XmppState,
		MonadError m, Error (ErrorType m) ) =>
	Pipe Common Common m ()
process = await >>= \mr -> case mr of
	Just (XCFeatures fts)
		| Just (FtMechanisms ms) <- find isFtMechanisms fts,
			Just n <- getMatched saslList ms ->
			sasl n >> mapM_ yield [XCDecl, begin] >> process
	Just (XCFeatures _fs) -> mapM_ yield binds >> process
	Just (SRPresence _ ns) -> case toCaps ns of
		C [(CTHash, "sha-1"), (CTVer, v), (CTNode, n)] ->
			yield (getCaps v n) >> process
		_ -> process
	Just (SRIq Get i (Just f) (Just (Jid u d _))
		(IqDiscoInfoNode [(DTNode, n)]))
		| (u, d) == let Jid u' d' _ = sender in (u', d') -> do
			yield $ resultCaps i f n
			yield . XCMessage Chat "prof_3" Nothing recipient .
				MBody $ MessageBody message
			yield XCEnd
	Just _ -> process
	_ -> return ()

begin :: Common
begin = XCBegin [(To, "localhost"), (Version, "1.0"), (Lang, "en")]

binds :: [Common]
binds = [SRIq Set "_xmpp_bind1" Nothing Nothing . IqBind Nothing $
		Resource "profanity",
	SRIq Set "_xmpp_session1" Nothing Nothing IqSession,
	SRIq Get "_xmpp_roster1" Nothing Nothing $ IqRoster Nothing,
	SRPresence [(Id, "prof_presence_1")] . fromCaps $
		capsToXmlCaps profanityCaps "http://www.profanity.im" ]

getCaps :: BS.ByteString -> BS.ByteString -> Common
getCaps v n = SRIq Get "prof_caps_2" Nothing (Just sender) $ IqCapsQuery v n

resultCaps :: BS.ByteString -> Jid -> BS.ByteString -> Common
resultCaps i t n = SRIq Result
	i Nothing (Just t) (IqCapsQuery2 [capsToQuery profanityCaps n])

message :: BS.ByteString
sender, recipient :: Jid
(sender, recipient, message) = unsafePerformIO $ do
	[s, r, m] <- getArgs
	return (toJid $ BSC.pack s, toJid $ BSC.pack r, BSC.pack m)

isProceed :: XmlNode -> Bool
isProceed (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed") _ [] [])
	= True
isProceed _ = False
