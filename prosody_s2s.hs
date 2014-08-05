{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.Pipe
import Text.XML.Pipe
import Network

import XmppClient

main :: IO ()
main = do
	h <- connectTo "localhost" (PortNumber 5269)
	void . runPipe $ (yield begin >> yield startTls) =$= output h
	void . runPipe $ handleP h
		=$= xmlEvent
		=$= convert fromJust
		=$= (xmlBegin >>= xmlNodeUntil isProceed)
--		=$= awaitAll
		=$= printP

awaitAll :: Monad m => Pipe a () m ()
awaitAll = await >>= maybe (return ()) (const awaitAll)

begin :: Common
begin = SRStreamSv [
	(From, "localhost"),
	(To, "localhost"),
	(Version, "1.0"),
	(Lang, "en")]

startTls :: Common
startTls = SRRaw $ XmlNode (("", Nothing), "starttls")
	[("", "urn:ietf:params:xml:ns:xmpp-tls")] [] []

isProceed :: XmlNode -> Bool
isProceed (XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-tls"), "proceed") _ [] [])
	= True
isProceed _ = False

printP :: Show a => Pipe a () IO ()
printP = await >>= maybe (return ()) (\x -> liftIO (print x) >> printP)
