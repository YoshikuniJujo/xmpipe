{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Network.Sasl.ScramSha1.ScramSha1 (
	clientFirstMessage,
	serverFirstMessage,
	clientFinalMessage,
	serverFinalMessage,

	readClientFirstMessage,
	readServerFirstMessage,
	readClientFinalMessage,
	readServerFinalMessage,

	exampleFlow,
	exampleServerFirstMessage,
	exampleServerFinalMessage,
	exampleProof,
	exampleSignature,
	exampleClientProof,
	exampleServerSignature,
	) where

import Control.Applicative
import Data.Bits

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.Hash.SHA1 as SHA1

import Network.Sasl.ScramSha1.Hmac
import Network.Sasl.ScramSha1.Fields

clientFirstMessage :: BS.ByteString -> BS.ByteString -> BS.ByteString
clientFirstMessage un nnc = "n,," `BS.append` clientFirstMessageBare un nnc

readClientFirstMessage :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
readClientFirstMessage rs = case BS.splitAt 3 rs of
	("n,,", rs') -> do
		let kv = readFields rs'
		(,) <$> lookup "n" kv <*> lookup "r" kv
	_ -> Nothing

clientFinalMessage :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int
	-> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
clientFinalMessage un ps slt i cb cnnc snnc = BS.concat [
	clientFinalMessageWithoutProof cb snnc, ",",
	"p=", clientProof un ps slt i cb cnnc snnc ]

readClientFinalMessage ::
 	BS.ByteString -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
readClientFinalMessage rs = do
	let kv = readFields rs
	(,,)	<$> ((\(Right r) -> r) . B64.decode <$> lookup "c" kv)
		<*> lookup "r" kv
		<*> ((\(Right r) -> r) . B64.decode <$> lookup "p" kv)

readServerFirstMessage :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString, Int)
readServerFirstMessage ch = do
	let kv = readFields ch
	(,,)	<$> lookup "r" kv
		<*> ((\(Right r) -> r) . B64.decode <$> lookup "s" kv)
		<*> (read . BSC.unpack <$> lookup "i" kv)

readServerFinalMessage :: BS.ByteString -> Maybe BS.ByteString
readServerFinalMessage = lookup "v" . readFields

serverFinalMessage :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int
	-> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
serverFinalMessage un ps slt i cb cnnc snnc = BS.concat
	["v=", serverSignature un ps slt i cb cnnc snnc]

testFlow :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int
	-> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
testFlow un ps slt i cb cnnc snnc = BS.concat [
	"C: ", clientFirstMessage un cnnc, "\n",
	"S: ", serverFirstMessage snnc slt i, "\n",
	"C: ", clientFinalMessage un ps slt i cb cnnc snnc, "\n",
	"S: ", serverFinalMessage un ps slt i cb cnnc snnc, "\n" ]

exampleServerFirstMessage :: BS.ByteString
exampleServerFirstMessage =
	serverFirstMessage exampleServerNonce exampleSalt exampleI

exampleServerFinalMessage :: BS.ByteString
exampleServerFinalMessage = serverFinalMessage
	"user" "pencil" exampleSalt exampleI "n,,"
	exampleClientNonce exampleServerNonce

exampleFlow :: BS.ByteString
exampleFlow = testFlow
	"user" "pencil" exampleSalt exampleI
	"n,," exampleClientNonce exampleServerNonce

int1 :: BS.ByteString
int1 = "\0\0\0\1"

xo :: BS.ByteString -> BS.ByteString -> BS.ByteString
"" `xo` b2 = b2
b1 `xo` "" = b1
b1 `xo` b2 = BS.pack $ zipWith xor (BS.unpack b1) (BS.unpack b2)

hi :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
hi str salt i =
	foldl1 xo . take i . tail . iterate (hmac SHA1.hash 64 str) $ salt `BS.append` int1

saltedPassword :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
saltedPassword = hi

clientKey :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
clientKey ps salt i = hmac SHA1.hash 64 (saltedPassword ps salt i) "Client Key"

storedKey :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
storedKey ps salt i = SHA1.hash $ clientKey ps salt i

clientFirstMessageBare :: BS.ByteString -> BS.ByteString -> BS.ByteString
clientFirstMessageBare un nnc = BS.concat ["n=", un, ",r=", nnc]

serverFirstMessage :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
serverFirstMessage snnc slt i = BS.concat
	["r=", snnc, ",s=", B64.encode slt, ",i=", BSC.pack $ show i]

clientFinalMessageWithoutProof :: BS.ByteString -> BS.ByteString -> BS.ByteString
clientFinalMessageWithoutProof cb snnc =
	BS.concat ["c=", B64.encode cb, ",r=", snnc]

authMessage :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
	-> BS.ByteString -> Int -> BS.ByteString
authMessage cb un cnnc snnc slt i = BS.concat [
	clientFirstMessageBare un cnnc, ",",
	serverFirstMessage snnc slt i, ",",
	clientFinalMessageWithoutProof cb snnc ]

clientSignature :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int
	-> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
clientSignature un ps slt i cb cnnc snnc =
	hmac SHA1.hash 64 (storedKey ps slt i) (authMessage cb un cnnc snnc slt i)

clientProof :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int
	-> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
clientProof un ps slt i cb cnnc snnc = B64.encode $
	clientKey ps slt i `xo` clientSignature un ps slt i cb cnnc snnc

serverKey :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
serverKey ps salt i = hmac SHA1.hash 64 (saltedPassword ps salt i) "Server Key"

serverSignature :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int
	-> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
serverSignature un ps slt i cb cnnc snnc = B64.encode $ hmac SHA1.hash 64
	(serverKey ps slt i)
	(authMessage cb un cnnc snnc slt i)

exampleClientProof :: BS.ByteString
exampleClientProof = B64.encode $ clientProof
	"user" "pencil" exampleSalt exampleI
	"n,," exampleClientNonce exampleServerNonce

exampleServerSignature :: BS.ByteString
exampleServerSignature = B64.encode $ serverSignature
	"user" "pencil" exampleSalt exampleI
	"n,," exampleClientNonce exampleServerNonce

exampleClientNonce :: BS.ByteString
exampleClientNonce = "fyko+d2lbbFgONRv9qkxdawL"

exampleServerNonce :: BS.ByteString
exampleServerNonce = exampleClientNonce `BS.append` "3rfcNHYJY1ZVvWVs7j"

exampleI :: Int
exampleI = 4096

exampleSalt :: BS.ByteString
Right exampleSalt = B64.decode "QSXCR+Q6sek8bf92"

exampleProof :: BS.ByteString
Right exampleProof = B64.decode "v0X8v3Bz2T0CJGbJQyF0X+HI4Ts="

exampleSignature :: BS.ByteString
Right exampleSignature = B64.decode "rmF9pqV8S7suAoZWja4dJRkFsKQ="
