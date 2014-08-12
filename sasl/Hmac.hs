module Hmac (hmac, bsToHex) where

import Data.Word
import Data.Bits
import Numeric

import qualified Data.ByteString as BS

type Hash = BS.ByteString -> BS.ByteString

ipad, opad :: [Word8]
ipad = repeat 0x36
opad = repeat 0x5c

hmac :: Hash -> Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
hmac h bs k = h . (makePad opad k' `BS.append`) . h . (makePad ipad k' `BS.append`)
	where
	k' = makeKey h bs k

makeKey :: Hash -> Int -> BS.ByteString -> BS.ByteString
makeKey h bs k
	| BS.length k > bs = makeKey h bs $ h k
	| otherwise = k `BS.append` BS.replicate (bs - BS.length k) 0

makePad :: [Word8] -> BS.ByteString -> BS.ByteString
makePad iop = BS.pack . mkp iop . BS.unpack
	where
	mkp _ [] = []
	mkp (p : ps) (k : ks) = p `xor` k : mkp ps ks
	mkp _ _ = error "makePad: bad"

bsToHex :: BS.ByteString -> String
bsToHex = concatMap (flip showHex "") . BS.unpack
