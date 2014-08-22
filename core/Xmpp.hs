{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module Xmpp (
	inputP2, inputP3, input, output, inputMpi, outputMpi,
	Mpi(..),

	Xmpp(..), fromCommon,
	Tags(..),
	Jid(..), toJid, fromJid,
	Side(..),
	Feature(..), Tag(..),
	Query(..), Bind(..), Requirement(..), toRequirement, fromRequirement,

	voidM, hlpDebug, SHandle(..),

	nullQ, tagsNull,
	) where

import Data.Maybe
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

import XmppType
import Tools

tagsNull :: Tags
tagsNull = Tags Nothing Nothing Nothing Nothing Nothing []

inputP2 :: Monad m => Pipe BS.ByteString Xmpp m ()
inputP2 = xmlEvent =$= convert fromJust =$= mapOut toCommon xmlReborn

isSaslSuccess :: XmlNode -> Bool
isSaslSuccess (XmlNode ((_, Just "jabber:client"), "iq")
	_ _ [XmlNode ((_, Just "urn:ietf:params:xml:ns:xmpp-bind"), "bind") _ _ _]) = True
isSaslSuccess _ = False

inputP3 :: Monad m => Pipe BS.ByteString Xmpp m [Xmlns]
inputP3 = xmlEvent =$= convert fromJust =$= mapOut toCommon xmlPipe

xmlPipe :: Monad m => Pipe XmlEvent XmlNode m [Xmlns]
xmlPipe = xmlBegin >>= \ns -> xmlNodeUntil isSaslSuccess ns >> return ns

input :: Monad m => [Xmlns] -> Pipe BS.ByteString Xmpp m ()
input ns = xmlEvent =$= convert fromJust =$= xmlNode ns =$= convert toCommon

inputMpi :: Monad m => [Xmlns] -> Pipe BS.ByteString Mpi m ()
inputMpi ns = xmlEvent =$= convert fromJust =$= xmlNode ns =$= convert toMpi

output :: Monad m => Pipe Xmpp BS.ByteString m ()
output = convert $ xmlString . (: []) . fromCommon Client

outputMpi :: Monad m => Pipe Mpi BS.ByteString m ()
outputMpi = convert $ xmlString . (: []) . fromMpi
