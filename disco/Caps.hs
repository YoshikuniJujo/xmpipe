{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Caps (
	Caps(..), Identity(..), mkHash, capsToXml, capsToQuery, profanityCaps,
	toCaps, capsToXmlCaps, fromCaps, CapsTag(..), XmlCaps(..)) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List
import Text.XML.Pipe
import Crypto.Hash.SHA1
import Data.ByteString.Base64

import qualified Data.ByteString as BS

data Caps = Caps [Identity] [BS.ByteString] deriving Show

data Identity = Identity {
	idCategory :: BS.ByteString,
	idType :: Maybe BS.ByteString,
	idLang :: Maybe BS.ByteString,
	idName :: Maybe BS.ByteString }
	deriving (Eq, Ord, Show)

serialize :: Identity -> BS.ByteString
serialize i = BS.concat [
	idCategory i, "/",
	fromMaybe "" $ idType i, "/",
	fromMaybe "" $ idLang i, "/",
	fromMaybe "" $ idName i ]

capsToXml :: Caps -> BS.ByteString -> XmlNode
capsToXml c n = XmlNode (nullQ "c")
	[("", "http://jabber.org/protocol/caps")]
	[(nullQ "hash", "sha-1"), (nullQ "node", n), (nullQ "ver", mkHash c)] []

mkHash :: Caps -> BS.ByteString
mkHash (Caps ids fs) = encode . hash . BS.concat . map (`BS.append` "<") $
	map serialize (sort ids) ++ sort fs

capsToQuery :: Caps -> BS.ByteString -> XmlNode
capsToQuery (Caps ids fs) nd = XmlNode (nullQ "query")
	[("", "http://jabber.org/protocol/disco#info")]
	[(nullQ "node", nd)]
	$ map identityToXml ids ++ map featureToXml fs

identityToXml :: Identity -> XmlNode
identityToXml i = XmlNode (nullQ "identity") [] (catMaybes [
	Just (nullQ "category", idCategory i),
	(nullQ "type" ,) <$> idType i,
	(nullQ "lang" ,) <$> idLang i,
	(nullQ "name" ,) <$> idName i ]) []

featureToXml :: BS.ByteString -> XmlNode
featureToXml f =
	XmlNode (nullQ "feature") [] [(nullQ "var", f)] []

_sampleId :: Identity
_sampleId = Identity {
	idCategory = "client",
	idType = Just "pc",
	idLang = Nothing,
	idName = Just "Exodus 0.9.1" }

_sampleFeatures :: [BS.ByteString]
_sampleFeatures = [
	"http://jabber.org/protocol/caps",
	"http://jabber.org/protocol/disco#info",
	"http://jabber.org/protocol/disco#items",
	"http://jabber.org/protocol/muc" ]

profanityCaps :: Caps
profanityCaps = Caps
	[Identity "client" (Just "console") Nothing (Just "Profanity 0.4.0")] [
		"http://jabber.org/protocol/caps",
		"http://jabber.org/protocol/chatstates",
		"http://jabber.org/protocol/disco#info",
		"http://jabber.org/protocol/disco#items",
		"http://jabber.org/protocol/muc",
		"jabber:iq:version",
		"urn:xmpp:ping" ]

data XmlCaps
	= C [(CapsTag, BS.ByteString)]
	| CapsRaw [XmlNode]
	deriving Show

toCaps :: [XmlNode] -> XmlCaps
toCaps [XmlNode ((_, Just "http://jabber.org/protocol/caps"), "c") _ as []] =
	C $ map (first toCapsTag) as
toCaps ns = CapsRaw ns

capsToXmlCaps :: Caps -> BS.ByteString -> XmlCaps
capsToXmlCaps c n = C [(CTHash, "sha-1"), (CTNode, n), (CTVer, mkHash c)]

fromCaps :: XmlCaps -> [XmlNode]
fromCaps (C ts) = (: []) $ XmlNode (nullQ "c")
	[("", "http://jabber.org/protocol/caps")] (map (first fromCapsTag) ts) []
fromCaps (CapsRaw ns) = ns

data CapsTag = CTHash | CTNode | CTVer | CTRaw QName deriving (Eq, Show)

toCapsTag :: QName -> CapsTag
toCapsTag ((_, Just "http://jabber.org/protocol/caps"), "hash") = CTHash
toCapsTag ((_, Just "http://jabber.org/protocol/caps"), "ver") = CTVer
toCapsTag ((_, Just "http://jabber.org/protocol/caps"), "node") = CTNode
toCapsTag n = CTRaw n

fromCapsTag :: CapsTag -> QName
fromCapsTag CTHash = nullQ "hash"
fromCapsTag CTVer = nullQ "ver"
fromCapsTag CTNode = nullQ "node"
fromCapsTag (CTRaw n) = n
