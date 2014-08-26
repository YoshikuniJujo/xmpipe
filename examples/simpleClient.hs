{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Prelude hiding (filter)

import Control.Applicative
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Writer
import Control.Concurrent hiding (yield)
import Data.Maybe
import Data.Pipe
import Data.Pipe.Flow
import Data.Pipe.ByteString
import System.IO
import System.Environment
import Text.XML.Pipe
import Network
import Network.Sasl
import Network.XMPiPe.Core.C2S.Client

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

mechanisms :: [BS.ByteString]
mechanisms = ["SCRAM-SHA-1", "DIGEST-MD5", "PLAIN"]

data St = St [(BS.ByteString, BS.ByteString)]
instance SaslState St where getSaslState (St ss) = ss; putSaslState ss _ = St ss

main :: IO ()
main = do
	(me_ : pw : you_ : _) <- map BSC.pack <$> getArgs
	let	me@(Jid un d (Just rsc)) = toJid me_; you = toJid you_
		ss = St [
			("username", un), ("authcid", un), ("password", pw),
			("cnonce", "00DEADBEEF00") ]
	h <- connectTo (BSC.unpack d) $ PortNumber 5222
	void . (`evalStateT` ss) . runPipe $
		fromHandle h =$= sasl d mechanisms =$= toHandle h
	(Just ns, _fts) <- runWriterT . runPipe $
		fromHandle h =$= bind d rsc =@= toHandle h
	void . forkIO . void . runPipe $ fromHandle h =$= input ns
		=$= convert fromMessage =$= filter isJust =$= convert fromJust
		=$= toHandleLn stdout
	void . (`runStateT` 0) . runPipe $ do
		yield (presence me) =$= output =$= toHandle h
		fromHandleLn stdin =$= before (== "/quit")
			=$= mkMessage you =$= output =$= toHandle h
		yield End =$= output =$= toHandle h

presence :: Jid -> Mpi
presence me = Presence
	(tagsNull { tagFrom = Just me }) [XmlNode (nullQ "presence") [] [] []]

mkMessage :: Jid -> Pipe BS.ByteString Mpi (StateT Int IO) ()
mkMessage you = (await >>=) . maybe (return ()) $ \m -> do
	n <- get; modify succ
	yield $ toM n m
	mkMessage you
	where toM n msg = Message (tagsType "chat") {
			tagId = Just . BSC.pack . ("msg_" ++) $ show n,
			tagTo = Just you }
		[XmlNode (nullQ "body") [] [] [XmlCharData msg]]

fromMessage :: Mpi -> Maybe BS.ByteString
fromMessage (Message ts [XmlNode _ [] [] [XmlCharData m]])
	| Just (Jid n d _) <- tagFrom ts = Just $ BS.concat [n, "@", d, ": ", m]
fromMessage _ = Nothing
