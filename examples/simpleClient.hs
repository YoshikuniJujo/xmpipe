{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Prelude hiding (filter)

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Writer
import Control.Concurrent hiding (yield)
import Data.Pipe
import Data.Pipe.Flow
import Data.Pipe.ByteString
import System.IO
import Text.XML.Pipe
import Network
import Network.Sasl
import Network.XMPiPe.Core.C2S.Client

import qualified Data.ByteString as BS

main :: IO ()
main = do
	h <- connectTo "localhost" $ PortNumber 5222
	_ <- (`runStateT` si) $
		runPipe $ fromHandle h
			=$= sasl "localhost" mechanisms
			=$= toHandle h
	(Just ns, _fts) <-
		runWriterT . runPipe $ fromHandle h
			=$= bind "localhost" "profanity"
			=@= toHandle h
	_ <- runPipe $ yield presence
		=$= output
		=$= toHandle h
	_ <- forkIO . void . runPipe $ fromHandle h -- =$= toHandleLn stdout
		=$= input ns
		=$= filter isMessage
		=$= convert fromMessageBody
		=$= toHandleLn stdout
	_ <- runPipe $ fromHandleLn stdin
		=$= before (== "/quit")
		=$= convert toMessage
		=$= output
		=$= toHandle h
	_ <- runPipe $ yield End =$= output =$= toHandle h
	return ()
	where
	si = saslState "yoshikuni" "password" "00DEADBEEF00"

fromHandle1 :: Handle -> Pipe () BS.ByteString IO ()
fromHandle1 h = do
	c <- lift $ BS.hGet h 1
	yield c
	fromHandle1 h

presence :: Mpi
presence = Presence
	(tagsNull { tagFrom = Just $ Jid "yoshikuni" "localhost" Nothing })
	[XmlNode (nullQ "presence") [] [] []]

isMessage :: Mpi -> Bool
isMessage (Message _ [XmlNode ((_, Just "jabber:client"), "body")
	[] [] [XmlCharData _]]) = True
isMessage _ = False

fromMessageBody :: Mpi -> BS.ByteString
fromMessageBody (Message ts [XmlNode _ [] [] [XmlCharData m]]) = let
	Just (Jid n d _) = tagFrom ts in
	BS.concat [n, "@", d, ": ", m]
fromMessageBody _ = error "bad"

toMessage :: BS.ByteString -> Mpi
toMessage m = Message (tagsType "chat") {
		tagId = Just "hoge",
		tagTo = Just (Jid "yoshio" "localhost" Nothing) }
	[XmlNode (nullQ "body") [] [] [XmlCharData m]]

saslState :: BS.ByteString -> BS.ByteString -> BS.ByteString -> St
saslState un pw cn = St
	[("username", un), ("authcid", un), ("password", pw), ("cnonce", cn)]

mechanisms :: [BS.ByteString]
mechanisms = ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]

data St = St [(BS.ByteString, BS.ByteString)]

instance SaslState St where
	getSaslState (St ss) = ss
	putSaslState ss _ = St ss
