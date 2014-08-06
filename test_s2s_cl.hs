{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

import Control.Concurrent.STM
import TestFederationCl
import Common

main :: IO ()
main = do
	(ca, k, c) <- readFiles
	(i, e) <- connect ca k c
	atomically . writeTChan i $ convertMessage sampleMessage
	atomically $ readTChan e

sampleMessage :: Common
sampleMessage = SRMessage Chat "hoge"
	(Just . Jid "yoshikuni" "localhost" $ Just "profanity")
	(Jid "yoshio" "otherhost" Nothing)
	(MBody $ MessageBody "HELLO")
