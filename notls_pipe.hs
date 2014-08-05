{-# LANGUAGE OverloadedStrings, TypeFamilies, PackageImports, FlexibleContexts #-}

import Debug.Trace

import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.List
import Data.Pipe
import Data.HandleLike
import System.Environment
import System.IO.Unsafe
import Network

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import XmppClient
import Caps (profanityCaps)
import qualified Caps as CAPS

host :: String
host = case BSC.unpack $ jidToHost sender of
	"otherhost" -> "localhost"
	h -> h

port :: PortID
port = PortNumber $ case jidToHost sender of
	"otherhost" -> 55222
	_ -> 5222

main :: IO ()
main = connectTo host port >>= \h ->
	xmpp (SHandle h) `evalStateT` ("" :: BS.ByteString)

xmpp :: (HandleLike h, MonadState (HandleMonad h),
		BS.ByteString ~ StateType (HandleMonad h)) => h -> HandleMonad h ()
xmpp h = voidM . runPipe $ input h =$= proc =$= output h

proc :: (Monad m, MonadState m, StateType m ~ BS.ByteString) =>
	Pipe Common Common m ()
proc = yield SRXmlDecl
	>> yield (SRStream [(To, "localhost"), (Version, "1.0"), (Lang, "en")])
	>> process

jidToHost :: Jid -> BS.ByteString
jidToHost (Jid _ d _) = d

process :: (Monad m, MonadState m, StateType m ~ BS.ByteString) =>
	Pipe Common Common m ()
process = await >>= \mr -> case mr of
	Just (SRFeatures [Mechanisms ms])
		| DigestMd5 `elem` ms -> do
			let Jid u _ _ = sender
			digestMd5 u
			process
	Just SRSaslSuccess -> mapM_ yield [SRXmlDecl, begin] >> process
	Just (SRFeatures fs) -> do
		trace "HERE" (return ())
		{-
		let Caps { cnode = n, cver = v } = case find isCaps fs of
			Just c -> c
			_ -> error $ "hoge: " ++ show fs
		trace (show $ (n, v)) (return ())
		-}
		mapM_ yield binds
--		yield $ getCaps "prof_caps_4492" Nothing v n
		process
	Just (SRPresence _ ns) -> do
		case toCaps ns of
			C [(CTHash, "sha-1"), (CTVer, v), (CTNode, n)] ->
				yield (getCaps "prof_caps_2" (Just sender) v n)
			_ -> return ()
		process
	Just (SRIq Get i (Just f) (Just (Jid u d _))
		(IqDiscoInfoNode [(DTNode, n)]))
		| (u, d) == let Jid u' d' _ = sender in (u', d') -> do
			yield $ resultCaps i f n
			yield discoIq
			yield . SRMessage Chat "prof_3" Nothing recipient .
				MBody $ MessageBody message
--			yield SREnd
			process
	Just (SRIq Result "info1" _ _ (IqDiscoInfoFull _ ids fs _)) -> do
		yield . SRMessage Chat "debug_1" Nothing recipient
			. MBody . MessageBody . BSC.pack . show
			. B64.decode . CAPS.mkHash
			. CAPS.Caps (map idToId ids) $ map fromInfoFeature fs
		yield SREnd
	Just _ -> process
	_ -> return ()

idToId :: Identity -> CAPS.Identity
idToId (Identity is) = CAPS.Identity {
	CAPS.idCategory = fromJust $ lookup IDTCategory is,
	CAPS.idType = lookup IDTType is,
	CAPS.idLang = lookup IDTLang is,
	CAPS.idName = lookup IDTName is
	}

fromInfoFeature :: InfoFeature -> BS.ByteString
fromInfoFeature (InfoFeature f) = f
fromInfoFeature _ = error "bad"

begin :: Common
begin = SRStream [(To, "localhost"), (Version, "1.0"), (Lang, "en")]

binds :: [Common]
binds = [SRIq Set "_xmpp_bind1" Nothing Nothing . IqBind Nothing $
		Resource "profanity",
	SRIq Set "_xmpp_session1" Nothing Nothing IqSession,
	SRIq Get "_xmpp_roster1" Nothing Nothing $ IqRoster Nothing,
	SRPresence [(Id, "prof_presence_1")] . fromCaps $
		capsToCaps profanityCaps "http://www.profanity.im" ]

getCaps :: BS.ByteString -> Maybe Jid -> BS.ByteString -> BS.ByteString ->
	Common
getCaps i (Just t) v n = SRIq Get i Nothing (Just t) $ IqCapsQuery v n
getCaps i _ v n = SRIq Get i Nothing Nothing $ IqCapsQuery v n

resultCaps :: BS.ByteString -> Jid -> BS.ByteString -> Common
resultCaps i t n =
	SRIq Result i Nothing (Just t) (IqCapsQuery2 profanityCaps n)

message :: BS.ByteString
sender, recipient :: Jid
(sender, recipient, message) = unsafePerformIO $ do
	[s, r, m] <- getArgs
	return (toJid $ BSC.pack s, toJid $ BSC.pack r, BSC.pack m)

discoIq :: Common
discoIq = SRRaw $ XmlNode (nullQ "iq") [] [
	(nullQ "type", "get"),
	(nullQ "from", "yoshikuni@localhost"),
	(nullQ "to", "localhost"),
	(nullQ "id", "info1") ] [discoQuery]

discoQuery :: XmlNode
discoQuery = XmlNode (nullQ "query")
	[("", "http://jabber.org/protocol/disco#info")] [] []
