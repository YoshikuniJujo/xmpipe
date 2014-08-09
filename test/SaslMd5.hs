{-# LANGUAGE OverloadedStrings, PackageImports #-}

module SaslMd5 (sasl) where

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5

import Sasl

exampleSasl :: (Monad m, SaslState s) => BS.ByteString -> Sasl s m
exampleSasl ps = S2C $ \getCh -> C2S $ \getRs -> Result $ do
	(ch, rs) <- (,) `liftM` getCh `ap` getRs
	return $ if rs == MD5.hash (ch `BS.append` ps)
		then (True, Nothing)
		else (False, Nothing)

exampleSend :: (Monad m, SaslState s) => Send BS.ByteString s m
exampleSend ps = [getResponse ps]

getResponse :: (Monad m, SaslState s) =>
	BS.ByteString -> StateT s m BS.ByteString
getResponse ps = do
	Just ch <- gets $ lookup "challenge" . getSaslState
	return . MD5.hash $ ch `BS.append` ps

exampleRecieve :: (Monad m, SaslState s) => Recieve s m
exampleRecieve = [setChallenge]

setChallenge :: (Monad m, SaslState s) => BS.ByteString -> StateT s m ()
setChallenge ch = modify $ putSaslState [("challenge", ch)]

sasl :: (Monad m, SaslState s) => BS.ByteString ->
	Pipe BS.ByteString BS.ByteString (StateT s m) ()
sasl = pipeCl (exampleSasl "password") exampleSend exampleRecieve
