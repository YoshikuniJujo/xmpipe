{-# LANGUAGE OverloadedStrings #-}

module DigestMd5 (
	DigestResponse(..), fromDigestResponse,
	digestMd5,

	DigestMd5Challenge(..), fromDigestMd5Challenge,
) where

import Control.Applicative
import Crypto.Hash.MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Numeric
import Data.Word

(+++) :: ByteString -> ByteString -> ByteString
(+++) = BS.append

hash32 :: ByteString -> ByteString
hash32 = BSC.pack . concatMap hex2 . BS.unpack . hash

hex2 :: Word8 -> String
hex2 w = replicate (2 - length s) '0' ++ s
	where
	s = showHex w ""

digestMd5 :: Bool -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString
	-> ByteString -> ByteString -> ByteString -> ByteString
digestMd5 isClient username rlm password q uri n nc cnonce = z
	where
	x = username +++ ":" +++ rlm +++ ":" +++ password
	y = hash x
	a1 = y +++ ":" +++ n +++ ":" +++ cnonce -- +++ ":" +++ authzid
	ha1 = hash32 a1
	a2 = (if isClient then "AUTHENTICATE" else "") +++ ":" +++ uri
	ha2 = hash32 a2
	kd = ha1 +++ ":" +++ n +++ ":" +++ nc +++ ":" +++ cnonce +++ ":" +++
		q +++ ":" +++ ha2
	z = hash32 kd

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

fromDigestResponse :: DigestResponse -> BS.ByteString
fromDigestResponse = kvsToS . responseToKvs True

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

calcMd5 :: Bool -> DigestResponse -> BS.ByteString
calcMd5 isClient = digestMd5 isClient
	<$> drUserName <*> drRealm <*> drPassword <*> drQop <*> drDigestUri
	<*> drNonce <*> drNc <*> drCnonce

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
