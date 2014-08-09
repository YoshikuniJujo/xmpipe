{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Sasl (
	Sasl(..), Send, Recieve, SaslState(..), pipeCl ) where

import "monads-tf" Control.Monad.State
import Data.Pipe

import qualified Data.ByteString as BS

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

pipeCl :: Monad m => Sasl s m -> Send a s m -> Recieve s m -> a ->
	Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeCl sasl send rcv x0 = ppc sasl (send x0) rcv

ppc :: Monad m => Sasl s m -> [StateT s m BS.ByteString] -> Recieve s m ->
	Pipe BS.ByteString BS.ByteString (StateT s m) ()
ppc (Result rslt) _ _ = do
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
ppc _ _ _ = error "bad"

class SaslState s where
	getSaslState :: s -> [(BS.ByteString, BS.ByteString)]
	putSaslState :: [(BS.ByteString, BS.ByteString)] -> s -> s
