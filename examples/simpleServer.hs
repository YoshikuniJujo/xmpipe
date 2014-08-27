{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

import Prelude hiding (until)

import Control.Applicative
import Control.Arrow
import Control.Monad
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Data.Pipe
import Data.Pipe.IO (debug)
import Data.Pipe.ByteString
import Data.Pipe.TChan
import Network
import Network.Sasl
import Network.XMPiPe.Core.C2S.Server

import qualified Data.ByteString as BS
import qualified Network.Sasl.DigestMd5.Server as DM5
import qualified Network.Sasl.ScramSha1.Server as SS1

main :: IO ()
main = do
	userlist <- atomically $ newTVar []
	soc <- listenOn $ PortNumber 5222
	forever $ accept soc >>= \(h, _, _) -> forkIO $ do
		(Just ns, st) <- (`runStateT` initState) $ do
			_ <- runPipe $ fromHandle h
				=$= debug
				=$= sasl "localhost" sampleRetrieves
				=$= debug
				=$= toHandle h
			runPipe $ fromHandle h
				=$= bind "localhost" []
				=@= toHandle h
		to <- atomically newTChan
		void . forkIO . void . runPipe $ fromTChan to
			=$= output
			=$= toHandle h
		atomically $ modifyTVar userlist ((userName st, to) :)
		void . runPipe $ fromHandle h
			=$= input ns
			=$= selectOut (userName st)
			=$= toTChansM (getOutputList userlist to)

getOutputList :: TVar [(Jid, TChan Mpi)]
	-> TChan Mpi -> IO [(Maybe Jid -> Bool, TChan Mpi)]
getOutputList ul to =
	((isToMe, to) :) . map (first same) <$> atomically (readTVar ul)

messageTo :: TVar [(Jid, TChan Mpi)] -> TChan Mpi -> Pipe (Maybe Jid, Mpi) () IO ()
messageTo vul to = do
	ul <- lift . atomically $ readTVar vul
	toTChans ((isToMe, to) : map (first same) ul)

same :: Jid -> Maybe Jid -> Bool
same (Jid u d _) (Just (Jid u' d' _)) = u == u' && d == d'
same _ _ = False

isToMe :: Maybe Jid -> Bool
isToMe (Just (Jid "" "localhost" Nothing)) = True
isToMe _ = False

selectOut :: Monad m => Jid -> Pipe Mpi (Maybe Jid, Mpi) m ()
selectOut fr = (await >>=) . maybe (return ()) $ \mpi -> case mpi of
	Message ts bd -> yield (tagTo ts, Message ts { tagFrom = Just fr } bd) >>
		selectOut fr
	End -> yield (Just $ Jid "" "localhost" Nothing, mpi)
	_ -> yield (Nothing, mpi) >> selectOut fr

initState :: XSt
initState = XSt {
	userName = Jid "" "localhost" Nothing,
	randomList = repeat "HELLO",
	sSt = [
		("realm", "localhost"),
		("qop", "auth"),
		("charset", "utf-8"),
		("algorithm", "md5-sess") ] }

sampleRetrieves :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => [Retrieve m]
sampleRetrieves =
	[RTPlain retrievePln, RTDigestMd5 retrieveDM5, RTScramSha1 retrieveSS1]

retrievePln :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	BS.ByteString -> BS.ByteString -> BS.ByteString -> m ()
retrievePln "" "yoshikuni" "password" = return ()
retrievePln "" "yoshio" "password" = return ()
retrievePln _ _ _ = throwError $
	fromSaslError NotAuthorized "incorrect username or password"

retrieveDM5 :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) => BS.ByteString -> m BS.ByteString
retrieveDM5 "yoshikuni" = return $ DM5.mkStored "yoshikuni" "localhost" "password"
retrieveDM5 "yoshio" = return $ DM5.mkStored "yoshio" "localhost" "password"
retrieveDM5 _ = throwError $
	fromSaslError NotAuthorized "incorrect username or password"

retrieveSS1 :: (
	MonadState m, SaslState (StateType m),
	MonadError m, SaslError (ErrorType m) ) =>
	BS.ByteString -> m (BS.ByteString, BS.ByteString, BS.ByteString, Int)
retrieveSS1 "yoshikuni" = return (slt, stk, svk, i)
	where slt = "pepper"; i = 4492; (stk, svk) = SS1.salt "password" slt i
retrieveSS1 "yoshio" = return (slt, stk, svk, i)
	where slt = "sugar"; i = 4492; (stk, svk) = SS1.salt "password" slt i
retrieveSS1 _ = throwError $
	fromSaslError NotAuthorized "incorrect username or password"

data XSt = XSt {
	userName :: Jid,
	randomList :: [BS.ByteString],
	sSt :: [(BS.ByteString, BS.ByteString)] }

instance XmppState XSt where
	getXmppState xs = (Just $ userName xs, randomList xs)
	putXmppState (Just usr, rl) xs = xs { userName = usr, randomList = rl }
	putXmppState _ _ = error "bad"

instance SaslState XSt where
	getSaslState XSt {
		userName = Jid un _ _, randomList = nnc : _, sSt = ss } =
		("username", un) : ("nonce", nnc) : ("snonce", nnc) : ss
	getSaslState _ = error "bad"
	putSaslState ss xs@XSt {
		userName = Jid _ d r, randomList = _ : rs } =
		case lookup "username" ss of
			Just un -> xs {
				userName = Jid un d r,
				randomList = rs,
				sSt = ss }
			_ -> error "bad"
	putSaslState _ _ = error "bad"
