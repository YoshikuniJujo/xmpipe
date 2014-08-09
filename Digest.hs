{-# LANGUAGE OverloadedStrings #-}

module Digest (
	DigestResponse(..),
--	responseToContent,
--	B64.encode,
--	kvsToS,
--	responseToKvs,
	calcMd5,

	lookupResponse, toDigestResponse, fromDigestResponse, toRspauth,
	DigestMd5Challenge(..), fromDigestMd5Challenge, toDigestMd5Challenge,
	) where

import Control.Applicative
import Data.Maybe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import DigestMd5
import Papillon

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

responseToContent :: DigestResponse -> BS.ByteString
responseToContent = B64.encode . kvsToS . responseToKvs True

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

calcMd5 :: Bool -> DigestResponse -> BS.ByteString
calcMd5 isClient = digestMd5 isClient
	<$> drUserName <*> drRealm <*> drPassword <*> drQop <*> drDigestUri
	<*> drNonce <*> drNc <*> drCnonce
