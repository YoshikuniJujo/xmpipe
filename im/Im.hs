{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module Im (
	FeatureR(..), nodeToFeatureR, featureToFeatureR, featureRToFeature,
	IRRoster(..), Roster(..), toIRRoster, fromIRRoster,
	) where

import Control.Applicative
import Data.List
import Text.XML.Pipe

import qualified Data.ByteString as BS

import Xmpp

data FeatureR
	= Ft Feature
	| FRRosterver Requirement
	| FRNode XmlNode
	deriving (Eq, Ord, Show)

featureToFeatureR :: Feature -> FeatureR
featureToFeatureR (FtRaw n)
	| Just r <- ftRosterver n = FRRosterver r
featureToFeatureR f = Ft f

nodeToFeatureR :: XmlNode -> FeatureR
nodeToFeatureR n
	| Just r <- ftRosterver n = FRRosterver r
	| otherwise = FRNode n

ftRosterver :: XmlNode -> Maybe Requirement
ftRosterver (XmlNode ((_, Just "urn:xmpp:features:rosterver"), "ver") _ [] r) =
	Just $ toRequirement r
ftRosterver _ = Nothing

featureRToFeature :: FeatureR -> Feature
featureRToFeature (Ft f) = f
featureRToFeature (FRNode n) = FtRaw n
featureRToFeature (FRRosterver r) = FtRaw $ fromFtRosterver r

fromFtRosterver :: Requirement -> XmlNode
fromFtRosterver r = XmlNode (nullQ "ver")
	[("", "urn:xmpp:features:rosterver")] [] [fromRequirement r]

data IRRoster = IRRoster (Maybe Roster) deriving Show

data Roster = Roster (Maybe BS.ByteString) [XmlNode] deriving Show

toIRRoster :: [XmlNode] -> Maybe IRRoster
toIRRoster [XmlNode ((_, Just "jabber:iq:roster"), "query") _ [] []] =
	Just $ IRRoster Nothing
toIRRoster [XmlNode ((_, Just "jabber:iq:roster"), "query") _ as ns] = Just .
	IRRoster . Just $ Roster (snd <$> find (\((_, v), _) -> v == "ver") as) ns
toIRRoster _ = Nothing

fromIRRoster :: IRRoster -> XmlNode
fromIRRoster (IRRoster Nothing) =
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] [] []
fromIRRoster (IRRoster (Just (Roster mv ns))) =
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] as ns
	where as = case mv of
		Just v -> [(nullQ "ver", v)]
		_ -> []
