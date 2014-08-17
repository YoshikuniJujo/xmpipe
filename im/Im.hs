{-# LANGUAGE OverloadedStrings #-}

module Im (
	FeatureR(..), featureToFeatureR, featureRToFeature) where

import Text.XML.Pipe

import Xmpp

data FeatureR
	= Ft Feature
	| FRRosterver Requirement
	deriving (Eq, Ord, Show)

featureToFeatureR :: Feature -> FeatureR
featureToFeatureR (FtRaw n)
	| Just r <- ftRosterver n = FRRosterver r
featureToFeatureR f = Ft f

ftRosterver :: XmlNode -> Maybe Requirement
ftRosterver (XmlNode ((_, Just "urn:xmpp:features:rosterver"), "ver") _ [] r) =
	Just $ toRequirement r
ftRosterver _ = Nothing

featureRToFeature :: FeatureR -> Feature
featureRToFeature (Ft f) = f
featureRToFeature (FRRosterver r) = FtRaw $ fromFtRosterver r

fromFtRosterver :: Requirement -> XmlNode
fromFtRosterver r = XmlNode (nullQ "ver")
	[("", "urn:xmpp:features:rosterver")] [] [fromRequirement r]
