{-# LANGUAGE OverloadedStrings, TypeFamilies, PackageImports #-}

module Digest (
	DigestMd5Challenge(..), fromDigestMd5Challenge,
	userName, getMd5, lookupResponse,

	digestMd5_,
	) where

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import DigestMd5
import Papillon

digestMd5_ :: (MonadState m, StateType m ~ BS.ByteString) =>
	BS.ByteString -> Pipe BS.ByteString BS.ByteString m ()
digestMd5_ sender = do
	mr <- await
	case mr of
		Just r -> do
			let [s] = digestMd5Data sender r
			lift . put $ getMd5 False s
			yield s
		_ -> error "digestMd5: unexpected end of input"
	mr' <- await
	case mr' of
		Just s -> do
			sa0 <- lift get
			let Just sa = toRspauth s
			unless (sa == sa0) $ error "process: bad server"
			mapM_ yield $ digestMd5Data sender s
		_ -> error "digestMd5: unexpected end of input"

digestMd5Data :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
digestMd5Data _ dmc | Just _ <- toRspauth dmc = [""]
digestMd5Data sender dmc = [fromDigestResponse dr]
	where
	DigestMd5Challenge r n q c _a = toDigestMd5Challenge dmc
	dr = DR {
		drUserName = sender, drRealm = r, drPassword = "password",
		drCnonce = "00DEADBEEF00", drNonce = n, drNc = "00000001",
		drQop = q, drDigestUri = "xmpp/localhost", drCharset = c }

fromJust' :: String -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' em _ = error em

data DigestMd5Challenge = DigestMd5Challenge {
	realm :: BS.ByteString,
	nonce :: BS.ByteString,
	qop :: BS.ByteString,
	charset :: BS.ByteString,
	algorithm :: BS.ByteString }
	deriving Show

toDigestMd5Challenge :: BS.ByteString -> DigestMd5Challenge
toDigestMd5Challenge d = let
	Just a = parseAtts d in
	DigestMd5Challenge {
		realm = fromJust' "3" $ lookup "realm" a,
		nonce = fromJust' "4" $ lookup "nonce" a,
		qop = fromJust' "5" $ lookup "qop" a,
		charset = fromJust' "6" $ lookup "charset" a,
		algorithm = fromJust' "7" $ lookup "algorithm" a }

fromDigestMd5Challenge :: DigestMd5Challenge -> BS.ByteString
fromDigestMd5Challenge c = BS.concat [
	"realm=", BSC.pack . show $ realm c, ",",
	"nonce=", BSC.pack . show $ nonce c, ",",
	"qop=", BSC.pack . show $ qop c, ",",
	"charset=", charset c, ",", "algorithm=", algorithm c ]

lookupResponse :: BS.ByteString -> BS.ByteString
lookupResponse bs = let Just a = parseAtts bs in fromJust $ lookup "response" a

toDigestResponse :: BS.ByteString -> DigestResponse
toDigestResponse bs = let
	Just a = parseAtts bs in
	DR {	drUserName = fromJust $ lookup "username" a,
		drRealm = fromJust $ lookup "realm" a,
		drPassword = "password",
		drCnonce = fromJust $ lookup "cnonce" a,
		drNonce = fromJust $ lookup "nonce" a,
		drNc = fromJust $ lookup "nc" a,
		drQop = fromJust $ lookup "qop" a,
		drDigestUri = fromJust $ lookup "digest-uri" a,
		drCharset = fromJust $ lookup "charset" a }

fromDigestResponse :: DigestResponse -> BS.ByteString
fromDigestResponse = kvsToS . responseToKvs True

toRspauth :: BS.ByteString -> Maybe BS.ByteString
toRspauth d = case parseAtts d of
	Just [("rspauth", ra)] -> Just ra
	_ -> Nothing

kvsToS :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
kvsToS [] = ""
kvsToS [(k, v)] = k `BS.append` "=" `BS.append` v
kvsToS ((k, v) : kvs) =
	k `BS.append` "=" `BS.append` v `BS.append` "," `BS.append` kvsToS kvs

responseToKvs :: Bool -> DigestResponse -> [(BS.ByteString, BS.ByteString)]
responseToKvs isClient rsp = [
	("username", quote $ drUserName rsp),
	("realm", quote $ drRealm rsp),
	("nonce", quote $ drNonce rsp),
	("cnonce", quote $ drCnonce rsp),
	("nc", drNc rsp),
	("qop", drQop rsp),
	("digest-uri", quote $ drDigestUri rsp),
	("response", calcMd5 isClient rsp),
	("charset", drCharset rsp)
	]

quote :: BS.ByteString -> BS.ByteString
quote = (`BS.append` "\"") . ("\"" `BS.append`)

data DigestResponse = DR {
	drUserName :: BS.ByteString,
	drRealm :: BS.ByteString,
	drPassword :: BS.ByteString,
	drCnonce :: BS.ByteString,
	drNonce :: BS.ByteString,
	drNc :: BS.ByteString,
	drQop :: BS.ByteString,
	drDigestUri :: BS.ByteString,
	drCharset :: BS.ByteString }
	deriving Show

userName :: BS.ByteString -> BS.ByteString
userName = drUserName . toDigestResponse

getMd5 :: Bool -> BS.ByteString -> BS.ByteString
getMd5 cl = calcMd5 cl . toDigestResponse

calcMd5 :: Bool -> DigestResponse -> BS.ByteString
calcMd5 isClient = digestMd5 isClient
	<$> drUserName <*> drRealm <*> drPassword <*> drQop <*> drDigestUri
	<*> drNonce <*> drNc <*> drCnonce
