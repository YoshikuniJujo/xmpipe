{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Common (
	toJid,
	fromJid,
	fromCommon,
	toCommon,
	Common(..), Tag(..), Mechanism(..),
	XmppCommon(..),
	Requirement(..),
	Feature(..), Bind(..), Jid(..), Query(..),
	Roster(..), Identity(..), IdentityTag(..),
	DiscoTag(..), InfoFeature(..), InfoFeatureTag(..),
	IqType(..),
	MessageBody(..),
	MessageDelay(..), DelayTag(..), toDelay,
	MessageXDelay(..), XDelayTag(..), toXDelay,
	MBody(..),
	MessageType(..),
	nullQ,
	Side(..),
	) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

import Digest
import Papillon

import XmppCommon
