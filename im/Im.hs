{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

module Im (
	FeatureR(..), featureToFeatureR, featureRToFeature,
	Im(..), readIm, outputIm,
	IRRoster(..),
	) where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.List
import Data.HandleLike
import Data.Pipe
import Text.XML.Pipe

import qualified Data.ByteString as BS

import Xmpp
import XmppType

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

data Im	= ImRoster BS.ByteString BS.ByteString IRRoster
	| ImMessage MessageType BS.ByteString (Maybe Jid) Jid MBody
	deriving Show

data IRRoster = IRRoster (Maybe Roster) deriving Show

readIm :: Xmpp -> Either Im Xmpp
readIm (SRIq Get i Nothing Nothing (QueryRaw ns))
	| Just ir <- readIRRoster ns = Left $ ImRoster "GET" i ir
readIm (XCRaw (XmlNode ((_, Just q), "message") _ as ns))
	| q `elem` ["jabber:client", "jabber:server"] =
		Left $ ImMessage tp i fr to $ toBody ns
	where
	ts = map (first toTag) as
	tp = toMessageType . fromJust $ lookup Type ts
	i = fromJust $ lookup Id ts
	fr = toJid <$> lookup From ts
	to = toJid . fromJust $ lookup To ts
	[] = filter ((`notElem` [Type, Id, From, To]) . fst) ts
readIm x = Right x

fromIm :: Im -> XmlNode
fromIm (ImRoster "RESULT" i ir) = XmlNode (nullQ "iq") []
	[(nullQ "type", "result"), (nullQ "id", i)] [fromRoster_ ir]
fromIm (ImRoster "GET" i ir) = XmlNode (nullQ "iq") []
	[(nullQ "type", "get"), (nullQ "id", i)] [fromRoster_ ir]
fromIm (ImMessage Chat i fr to (MBodyRaw ns)) =
	XmlNode (nullQ "message") [] (catMaybes [
		Just (fromTag Type, "chat"),
		Just (fromTag Id, i),
		(fromTag From ,) . fromJid <$> fr,
		Just (fromTag To, fromJid to) ]) ns
fromIm (ImMessage tp i fr to (MBody m)) =
	XmlNode (nullQ "message") []
		(catMaybes [
			Just $ messageTypeToAtt tp,
			Just (nullQ "id", i),
			(nullQ "from" ,) . fromJid <$> fr,
			Just (nullQ "to", fromJid to) ])
		[XmlNode (nullQ "body") [] [] [XmlCharData m]]
fromIm _ = error "bad"

fromRoster_ :: IRRoster -> XmlNode
fromRoster_ (IRRoster Nothing) =
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] [] []
fromRoster_ (IRRoster (Just (Roster mv ns))) =
	XmlNode (nullQ "query") [("", "jabber:iq:roster")] as ns
	where as = case mv of
		Just v -> [(nullQ "ver", v)]
		_ -> []

outputIm :: HandleLike h => h -> Pipe Im () (HandleMonad h) ()
outputIm h = (await >>=) . maybe (return ()) $ \n -> (>> outputIm h) $ do
	lift (hlPut h $ xmlString [fromIm n])

readIRRoster :: [XmlNode] -> Maybe IRRoster
readIRRoster [XmlNode ((_, Just "jabber:iq:roster"), "query") _ [] []] =
	Just $ IRRoster Nothing
readIRRoster [XmlNode ((_, Just "jabber:iq:roster"), "query") _ as ns] = Just .
	IRRoster . Just $ Roster (snd <$> find (\((_, v), _) -> v == "ver") as) ns
readIRRoster _ = Nothing
