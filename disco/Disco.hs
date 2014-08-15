{-# LANGUAGE OverloadedStrings #-}

module Disco (
	QueryDisco(..), DiscoTag(..), toQueryDisco, fromQueryDisco,
) where

import Control.Arrow
import Data.Maybe
import Text.XML.Pipe

import qualified Data.ByteString as BS

data QueryDisco
	= IqDiscoInfoNode [(DiscoTag, BS.ByteString)]
	| IqDiscoInfoFull [(DiscoTag, BS.ByteString)] [Identity] [InfoFeature] [XmlNode]
	| IqCapsQuery BS.ByteString BS.ByteString
	| IqCapsQuery2 [XmlNode]
	deriving Show

toQueryDisco :: [XmlNode] -> Maybe QueryDisco
toQueryDisco [XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "query")
	_ as []] = Just . IqDiscoInfoNode $ map (first toDiscoTag) as
toQueryDisco [XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "query")
	_ as ns] = Just $ IqDiscoInfoFull
	(map (first toDiscoTag) as)
	(mapMaybe toIdentity ns)
	(mapMaybe toInfoFeature ns)
	(filter (\n -> isNothing (toIdentity n) && isNothing (toInfoFeature n)) ns)
toQueryDisco _ = Nothing

fromQueryDisco :: QueryDisco -> [XmlNode]
fromQueryDisco (IqCapsQuery v n) = [capsQuery v n]
fromQueryDisco (IqCapsQuery2 ns) = ns
fromQueryDisco _ = error "yet"

data DiscoTag = DTNode | DTRaw QName deriving (Eq, Show)

toDiscoTag :: QName -> DiscoTag
toDiscoTag ((_, Just "http://jabber.org/protocol/disco#info"), "node") = DTNode
toDiscoTag n = DTRaw n

data Identity
	= Identity [(IdentityTag, BS.ByteString)]
	| IdentityRaw XmlNode
	deriving Show

toIdentity :: XmlNode -> Maybe Identity
toIdentity (XmlNode ((_, Just "http://jabber.org/protocol/disco#info"), "identity")
	_ as []) = Just . Identity $ map (first toIdentityTag) as
toIdentity _n = Nothing

data IdentityTag
	= IDTType | IDTName | IDTCategory | IDTLang | IDTRaw QName deriving (Eq, Show)

toIdentityTag :: QName -> IdentityTag
toIdentityTag ((_, Just "http://jabber.org/protocol/disco#info"), "type") = IDTType
toIdentityTag ((_, Just "http://jabber.org/protocol/disco#info"), "name") = IDTName
toIdentityTag ((_, Just "http://jabber.org/protocol/disco#info"), "category") =
	IDTCategory
toIdentityTag n = IDTRaw n

data InfoFeature
	= InfoFeature BS.ByteString
	| InfoFeatureSemiRaw [(InfoFeatureTag, BS.ByteString)]
	| InfoFeatureRaw XmlNode
	deriving Show

toInfoFeature :: XmlNode -> Maybe InfoFeature
toInfoFeature (XmlNode ((_, Just "http://jabber.org/protocol/disco#info"),
	"feature") _ as []) = Just $ case map (first toInfoFeatureTag) as of
		[(IFTVar, v)] -> InfoFeature v
		atts -> InfoFeatureSemiRaw atts
toInfoFeature _n = Nothing

data InfoFeatureTag = IFTVar | IFTVarRaw QName deriving (Eq, Show)

toInfoFeatureTag :: QName -> InfoFeatureTag
toInfoFeatureTag ((_, Just "http://jabber.org/protocol/disco#info"), "var") = IFTVar
toInfoFeatureTag n = IFTVarRaw n

capsQuery :: BS.ByteString -> BS.ByteString -> XmlNode
capsQuery v n = XmlNode (("", Nothing), "query")
	[("", "http://jabber.org/protocol/disco#info")]
	[((("", Nothing), "node"), n `BS.append` "#" `BS.append` v)] []
