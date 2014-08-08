{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Crypto.Hash.MD5 as MD5

data Sasl s m
	= C2S (StateT s m BS.ByteString -> Sasl s m)
	| S2C (StateT s m BS.ByteString -> Sasl s m)
	| Result (StateT s m (Bool, Maybe BS.ByteString))

type Send a s m = a -> [StateT s m BS.ByteString]
type Recieve s m = [BS.ByteString -> StateT s m ()]

client2server :: Sasl s m -> StateT s m BS.ByteString -> Sasl s m
client2server (C2S f) bs = f bs
client2server s@(S2C _) _ = s
client2server _ _ = error "bad"

server2client :: Sasl s m -> StateT s m BS.ByteString -> Sasl s m
server2client (S2C f) bs = f bs
server2client s@(C2S _) _ = s
server2client _ _ = error "bad"

pipeC :: Monad m => Sasl s m -> Send a s m -> Recieve s m -> a ->
	Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeC sasl send rcv x0 = ppc sasl (send x0) rcv

ppc :: Monad m => Sasl s m -> [StateT s m BS.ByteString] -> Recieve s m ->
	Pipe BS.ByteString BS.ByteString (StateT s m) ()
ppc (Result rslt) [] [] = do
	r <- lift rslt
	case r of
		(True, _) -> yield "success"
		_ -> yield "failure"
ppc sasl (send : sends) (rcv : rcvs) = await >>= \mbs -> case mbs of
	Just bs -> do
		lift $ rcv bs
		let sasl' = server2client sasl $ rcv bs >> return bs
		s <- lift send
		let sasl'' = client2server sasl' send
		yield s
		ppc sasl'' sends rcvs
	_ -> return ()
ppc sasl sends rcvs = case sasl of
	Result _ -> error $ "sends is null? " ++ show (null sends) ++ "\n" ++
		"rcvs is null? " ++ show (null rcvs)
	_ -> error "sasl is not result"

exampleSasl :: Monad m => BS.ByteString -> Sasl ExampleState m
exampleSasl ps = S2C $ \getCh -> C2S $ \getRs -> Result $ do
	(ch, rs) <- (,) `liftM` getCh `ap` getRs
	return $ if rs == MD5.hash (ch `BS.append` ps)
		then (True, Nothing)
		else (False, Nothing)

data ExampleState = ExampleState {
	password :: BS.ByteString,
	challenge :: Maybe BS.ByteString }

exampleSend :: Monad m => Send BS.ByteString ExampleState m
exampleSend ps = [getResponse ps]

getResponse :: Monad m => BS.ByteString -> StateT ExampleState m BS.ByteString
getResponse ps = do
	Just ch <- gets challenge
	return . MD5.hash $ ch `BS.append` ps

exampleRecieve :: Monad m => Recieve ExampleState m
exampleRecieve = [setChallenge]

setChallenge :: Monad m => BS.ByteString -> StateT ExampleState m ()
setChallenge ch = do
	st <- get
	put $ st { challenge = Just ch }

examplePipe :: Monad m => BS.ByteString ->
	Pipe BS.ByteString BS.ByteString (StateT ExampleState m) ()
examplePipe = pipeC (exampleSasl "password") exampleSend exampleRecieve

fromStdin :: MonadIO m => Pipe () BS.ByteString m ()
fromStdin = liftIO BS.getLine >>= yield

toStdout :: MonadIO m => Pipe BS.ByteString () m ()
-- toStdout = await >>= maybe (return ()) (\s -> lift (BSC.putStrLn s) >> toStdout)
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)

exampleInitState :: ExampleState
exampleInitState = ExampleState {
	password = "password",
	challenge = Nothing
	}

main :: IO ()
main = do
	ps <- BS.getLine
	ret <- (`evalStateT` exampleInitState) . runPipe $
		fromStdin =$= examplePipe ps =$= toStdout
	case ret of
		Just _ -> return ()
		_ -> putStrLn "error"
