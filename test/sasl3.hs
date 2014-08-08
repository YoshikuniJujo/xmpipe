{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State

import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5

data Sasl s
	= C2S (State s BS.ByteString -> Sasl s)
	| S2C (State s BS.ByteString -> Sasl s)
	| Success (Maybe BS.ByteString)
	| Failure (Maybe BS.ByteString)

type MakeResponse a s = a -> [State s BS.ByteString]
type MakeChallenge a s = a -> [State s BS.ByteString]

data ExampleState = ExampleState {
	password :: BS.ByteString,
	challenge :: Maybe BS.ByteString
	} deriving Show

initState :: BS.ByteString -> ExampleState
initState ps = ExampleState {
	password = ps,
	challenge = Nothing
	}

exampleSasl :: BS.ByteString -> Sasl ExampleState
exampleSasl ps = S2C $ \getCh -> C2S $ \getRs -> let
	(ch, rs) = (`evalState` initState ps) $ (,) <$> getCh <*> getRs in
	if rs == MD5.hash (ch `BS.append` ps)
		then Success Nothing
		else Failure Nothing

exampleMkRsp :: MakeResponse BS.ByteString ExampleState
exampleMkRsp ps = [getResponse ps]

exampleMkCh :: MakeChallenge BS.ByteString ExampleState
exampleMkCh ch = [setChallenge ch]

client2server :: Sasl s -> (State s BS.ByteString) -> Sasl s
client2server (C2S f) bs = f bs
client2server s@(S2C _) _ = s
client2server _ _ = error "bad"

server2client :: Sasl s -> (State s BS.ByteString) -> Sasl s
server2client (S2C f) bs = f bs
server2client s@(C2S _) _ = s
server2client _ _ = error "bad"

main_ :: IO ()
main_ = do
	let	s0 = exampleSasl "password"
		s1 = server2client s0 $ return "challenge"
	inp <- BS.getLine
	case client2server s1 . return $ MD5.hash $ "challenge" `BS.append` inp of
		Success _ -> putStrLn "succeed"
		Failure _ -> putStrLn "failure"

setChallenge :: BS.ByteString -> State ExampleState BS.ByteString
setChallenge ch = do
	st <- get
	put $ st { challenge = Just ch }
	return ch

getResponse :: BS.ByteString -> State ExampleState BS.ByteString
getResponse ps = do
	Just ch <- gets challenge
	return . MD5.hash $ ch `BS.append` ps

main :: IO ()
main = do
	let s0 = exampleSasl "password"
	ch <- BS.getLine
	let	s1 = server2client s0 $ setChallenge ch
		s2 = client2server s1 $ getResponse "password"
	case s2 of
		Success _ -> putStrLn "succeed"
		Failure _ -> putStrLn "failure"
