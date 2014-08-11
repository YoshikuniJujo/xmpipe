{-# LANGUAGE OverloadedStrings, PackageImports, FlexibleContexts #-}

import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import Data.Pipe
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import DigestMd5
import Papillon

fromFile :: (MonadBaseControl IO m, MonadIO m) =>
	FilePath -> Pipe () BS.ByteString m ()
fromFile fp = bracket (liftIO $ openFile fp ReadMode) (liftIO . hClose) fromHandle

fromHandle :: MonadIO m => Handle -> Pipe () BS.ByteString m ()
fromHandle h = liftIO (BS.hGetLine h) >>= (>> fromHandle h) . yield

toStdout :: MonadIO m => Pipe BS.ByteString () m ()
toStdout = await >>= maybe (return ())
	((>> toStdout) . liftIO . BSC.putStrLn . BSC.pack . show)

main :: IO ()
main = do
	r <- (`runStateT` ExampleState exampleInit) $ do
		runPipe $ fromFile "digestMd5cl.txt"
			=$= pipeSv server client
			=$= toStdout
	print r

pipeSv :: Monad m => [StateT s m BS.ByteString]
	-> [BS.ByteString -> StateT s m ()]
	-> Pipe BS.ByteString BS.ByteString (StateT s m) ()
pipeSv [] [] = return ()
pipeSv [send] [] = lift send >>= yield
pipeSv (send : sends) (rcv : rcvs) = do
	lift send >>= yield
	await >>= \mbs -> case mbs of
		Just bs -> do
			lift $ rcv bs
			pipeSv sends rcvs
		_ -> return ()

server :: (Monad m, SaslState s) => [StateT s m BS.ByteString]
server = [mkChallenge, mkRspAuth, mkResult]

exampleInit :: [(BS.ByteString, BS.ByteString)]
exampleInit = [
	("password", "password"),
	("realm", "localhost"),
	("nonce", "7658cddf-0e44-4de2-87df-41323bce97f4"),
	("qop", "auth"),
	("charset", "utf-8"),
	("algorithm", "md5-sess")
	]

mkChallenge, mkRspAuth, mkResult ::
	(Monad m, SaslState s) => StateT s m BS.ByteString
mkChallenge = do
	st <- gets getSaslState
	let	Just ps = lookup "password" st
		Just rlm = lookup "realm" st
		Just n = lookup "nonce" st
		Just q = lookup "qop" st
		Just c = lookup "charset" st
		Just a = lookup "algorithm" st
	return $ fromDigestMd5Challenge $ DigestMd5Challenge {
		realm = rlm,
		nonce = n,
		qop = q,
		charset = c,
		algorithm = a }

mkRspAuth = do
	st <- gets getSaslState
	let	Just un = lookup "username" st
		Just rlm = lookup "realm" st
		Just ps = lookup "password" st
		Just q = lookup "qop" st
		Just uri = lookup "digest-uri" st
		Just n = lookup "nonce" st
		Just nc = lookup "nc" st
		Just cn = lookup "cnonce" st
		Just rsp = lookup "response" st
		clc = digestMd5 True un rlm ps q uri n nc cn
		cls = digestMd5 False un rlm ps q uri n nc cn
	unless (clc == rsp) $ error "mkRspAuth: bad"
	return $ "rspauth=" `BS.append` cls

mkResult = return "success"

client :: (Monad m, SaslState s) => [BS.ByteString -> StateT s m ()]
client = [putResponse, \"" -> return ()]

putResponse :: (Monad m, SaslState s) => BS.ByteString -> StateT s m ()
putResponse bs = do
	st <- gets getSaslState
	let	Just rs = parseAtts bs
		Just rlm = lookup "realm" rs
		Just n = lookup "nonce" rs
		Just q = lookup "qop" rs
		Just c = lookup "charset" rs
--		Just a = lookup "algorithm" rs
		Just un = lookup "username" rs
		Just uri = lookup "digest-uri" rs
		Just cn = lookup "cnonce" rs
		Just nc = lookup "nc" rs
		Just rsp = lookup "response" rs
	modify . putSaslState $ [
		("realm", rlm),
		("nonce", n),
		("qop", q),
		("charset", c),
--		("algorithm", a),
		("username", un),
		("digest-uri", uri),
		("cnonce", cn),
		("nc", nc),
		("response", rsp) ] ++ st

class SaslState s where
	getSaslState :: s -> [(BS.ByteString, BS.ByteString)]
	putSaslState :: [(BS.ByteString, BS.ByteString)] -> s -> s

data DigestMd5Challenge = DigestMd5Challenge {
	realm :: BS.ByteString,
	nonce :: BS.ByteString,
	qop :: BS.ByteString,
	charset :: BS.ByteString,
	algorithm :: BS.ByteString }
	deriving Show

fromDigestMd5Challenge :: DigestMd5Challenge -> BS.ByteString
fromDigestMd5Challenge c = BS.concat [
	"realm=", BSC.pack . show $ realm c, ",",
	"nonce=", BSC.pack . show $ nonce c, ",",
	"qop=", BSC.pack . show $ qop c, ",",
	"charset=", charset c, ",", "algorithm=", algorithm c ]

data ExampleState = ExampleState [(BS.ByteString, BS.ByteString)]
	deriving Show

instance SaslState ExampleState where
	getSaslState (ExampleState s) = s
	putSaslState s _ = ExampleState s
