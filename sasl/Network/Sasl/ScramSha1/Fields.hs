{-# LANGUAGE OverloadedStrings #-}

module Network.Sasl.ScramSha1.Fields (readFields) where

import qualified Data.ByteString.Char8 as BSC

readFields :: BSC.ByteString -> [(BSC.ByteString, BSC.ByteString)]
readFields = map sepKV . sepByComma

sepKV :: BSC.ByteString -> (BSC.ByteString, BSC.ByteString)
sepKV bs = case BSC.span (/= '=') bs of
	(k, "") -> (k, "")
	(k, v) -> (k, BSC.tail v)

sepByComma :: BSC.ByteString -> [BSC.ByteString]
sepByComma bs = case BSC.span (/= ',') bs of
	(itm, "") -> [itm]
	(itm, rst) -> itm : sepByComma (BSC.tail rst)
