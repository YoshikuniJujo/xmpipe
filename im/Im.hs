{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Im (
	FeatureR(..), featureToFeatureR, featureRToFeature,
	IRRoster(..), readRoster, outputRoster,
	) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Data.List
import Data.HandleLike
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

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

data IRRoster = IRRoster (Maybe Roster) deriving Show

readRoster :: Xmpp -> Either (BS.ByteString, BS.ByteString, IRRoster) Xmpp
readRoster (SRIq Get i Nothing Nothing (QueryRaw ns))
	| Just ir <- readIRRoster ns = Left ("GET", i, ir)
readRoster x = Right x

fromRoster :: (BS.ByteString, BS.ByteString, IRRoster) -> XmlNode
fromRoster ("RESULT", i, ir) = XmlNode (nullQ "iq") []
	[(nullQ "type", "result"), (nullQ "id", i)] [fromRoster_ ir]
fromRoster _ = error "bad"

fromRoster_ :: IRRoster -> XmlNode
fromRoster_ (IRRoster Nothing) =
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] [] []
fromRoster_ (IRRoster (Just (Roster mv ns))) =
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] as ns
	where as = case mv of
		Just v -> [(nullQ "ver", v)]
		_ -> []

outputRoster :: HandleLike h => h -> Pipe (BS.ByteString, BS.ByteString, IRRoster) () (HandleMonad h) ()
outputRoster h = (await >>=) . maybe (return ()) $ \n -> (>> outputRoster h) $ do
	lift (hlPut h $ xmlString [fromRoster n])

readIRRoster :: [XmlNode] -> Maybe IRRoster
readIRRoster [XmlNode ((_, Just "jabber:iq:roster"), "query") _ [] []] =
	Just $ IRRoster Nothing
readIRRoster [XmlNode ((_, Just "jabber:iq:roster"), "query") _ as ns] = Just .
	IRRoster . Just $ Roster (snd <$> find (\((_, v), _) -> v == "ver") as) ns
readIRRoster _ = Nothing
