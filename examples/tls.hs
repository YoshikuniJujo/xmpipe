{-# LANGUAGE OverloadedStrings, ScopedTypeVariables,
	TypeFamilies, FlexibleContexts, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Writer
import "monads-tf" Control.Monad.Error
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Data.Maybe
import Data.Pipe
import Data.Pipe.Flow
import Data.Pipe.IO (debug)
import Data.Pipe.ByteString
import Data.Pipe.TChan
import System.Environment
import System.IO.Unsafe
import System.IO
import Text.XML.Pipe
import Network
import Network.Sasl
import Network.PeyoTLS.TChan.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.XMPiPe.Core.C2S.Client
import Im (IRRoster(..), FeatureR(..), fromIRRoster, nodeToFeatureR)
import Caps (
	XmlCaps(..), CapsTag(..), capsToQuery, profanityCaps, toCaps, fromCaps,
	capsToXmlCaps )
import Disco (QueryDisco(..), DiscoTag(..), fromQueryDisco, toQueryDisco)

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

main :: IO ()
main = do
	ca <- readCertificateStore ["certs/cacert.sample_pem"]
	k <- readKey "certs/xmpp_client.sample_key"
	c <- readCertificateChain ["certs/xmpp_client.sample_cert"]
	(g :: SystemRNG) <- cprgCreate <$> createEntropyPool
	h <- connectTo (BSC.unpack host) port
	_ <- runPipe $ fromHandle h =$= starttls host =$= toHandle h
	(inc, otc) <- open' h (BSC.unpack host) cipherSuites [(k, c)] ca g
	_ <- (`runStateT` si) $
		runPipe $ fromTChan inc =$= sasl host mechanisms =$= toTChan otc
	(Just ns, fts) <-
		runWriterT . runPipe $ fromTChan inc =$= bind host =@= toTChan otc
	sc <- atomically newTChan
	ic <- atomically newTChan
	_ <- forkIO . (>> return ()) . runPipe $ fromTChan inc
		=$= input ns
		=$= debug
		=$= (putPresence fts >> process)
		=$= toTChan sc
	_ <- forkIO . (>> return ()) . runPipe $ fromTChans [sc, ic]
		=$= output
		=$= toTChan otc
	_ <- runPipe $ fromHandleLn stdin
		=$= before (== "/quit")
		=$= convert toMessageMpi
		=$= (toTChan ic :: Pipe Mpi () IO ())
	atomically $ writeTChan ic End
	where
	si = saslState ((\(Jid u _ _) -> u) sender) "password" "00DEADBEEF00"

putPresence :: (MonadError m, Error (ErrorType m)) => [Feature] -> Pipe a Mpi m ()
putPresence fts = do
	mapM_ yield $ mapMaybe (resp . nodeToFeatureR) fts
	yield . Presence tagsNull { tagId = Just "prof_presence_1" }
		. fromCaps
		$ capsToXmlCaps profanityCaps "http://www.profanity.im"
	where
	resp (FRRosterver _) = Just $ Iq
		(tagsType "get") { tagId = Just "_xmpp-roster1" }
		[fromIRRoster $ IRRoster Nothing]
	resp _ = Nothing

process :: (MonadError m, Error (ErrorType m)) => Pipe Mpi Mpi m ()
process = await >>= \mr -> case mr of
	Just (Presence _ ns) -> case toCaps ns of
		C [(CTHash, "sha-1"), (CTVer, v), (CTNode, n)] ->
			yield (getCaps v n) >> process
		_ -> process
	Just (Iq Tags {
			tagType = Just "get", tagId = Just i,
			tagFrom = Just f, tagTo = Just (Jid u d _) } ns)
		| Just (IqDiscoInfoNode [(DTNode, n)]) <- toQueryDisco ns,
			(u, d) == let Jid u' d' _ = sender in (u', d') -> do

			yield $ resultCaps i f n

			yield $ Message (tagsType "chat") {
					tagId = Just "prof_3",
					tagTo = Just recipient }
				[XmlNode (nullQ "body") [] [] [XmlCharData message]]
			process
	Just _ -> process
	_ -> return ()

toMessageMpi :: BS.ByteString -> Mpi
toMessageMpi m = Message (tagsType "chat") {
	tagId = Just "hoge", tagTo = Just recipient }
	[XmlNode (nullQ "body") [] [] [XmlCharData m]]

getCaps :: BS.ByteString -> BS.ByteString -> Mpi
getCaps v n = Iq (tagsType "get") {
		tagId = Just "prof_caps_2", tagTo = Just sender }
	. fromQueryDisco $ IqCapsQuery v n

resultCaps :: BS.ByteString -> Jid -> BS.ByteString -> Mpi
resultCaps i t n = Iq (tagsType "result") { tagId = Just i, tagTo = Just t }
	. fromQueryDisco $ IqCapsQuery2 [capsToQuery profanityCaps n]

saslState :: BS.ByteString -> BS.ByteString -> BS.ByteString -> St
saslState un pw cn = St []
	[ ("username", un), ("authcid", un), ("password", pw), ("cnonce", cn) ]

data St = St {
	stFeatures :: [Feature],
	stSaslState :: [(BS.ByteString, BS.ByteString)] }
	deriving Show

instance SaslState St where
	getSaslState (St _ ss) = ss
	putSaslState ss (St fts _) = St fts ss
