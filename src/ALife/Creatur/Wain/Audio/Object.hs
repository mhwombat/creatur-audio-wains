------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.Object
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with objects that could be either wains or
-- audio samples.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Audio.Object
  (
    Object(..),
    isPattern,
    objectId,
    objectNum,
    objectAppearance,
    objectEnergy,
    objectChildEnergy,
    addIfWain,
    objectToWain
  ) where

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import qualified ALife.Creatur.Wain.GeneticSOM as S
import ALife.Creatur.Wain.Response (Response)
import ALife.Creatur.Wain.Audio.Pattern (Pattern)
import ALife.Creatur.Wain.Audio.Wain (PatternWain)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Lens
import GHC.Generics (Generic)
import Data.Serialize

data Object a rt = PObject Pattern String
              | AObject (PatternWain a rt)
              deriving (Eq, Show, Generic)

instance (Ord a, Serialize a, Serialize rt, S.Tweaker rt,
  Response a ~ S.Pattern rt)
    => Serialize (Object a rt)

isPattern :: Object a rt -> Bool
isPattern (PObject _ _) = True
isPattern (AObject _) = False

objectId :: Object a rt -> String
objectId (PObject _ s) = "Audio " ++ s
objectId (AObject a) = agentId a

objectNum :: Object a rt -> Int
objectNum (PObject _ s) = read . take 1 . drop 1 $ s
objectNum (AObject _) = 10

objectAppearance :: Object a rt -> Pattern
objectAppearance (PObject img _) = img
objectAppearance (AObject a) = view W.appearance a

objectEnergy :: Object a rt -> UIDouble
objectEnergy (PObject _ _) = 0
objectEnergy (AObject a) = view W.energy a

objectChildEnergy :: Object a rt -> Double
objectChildEnergy (PObject _ _) = 0
objectChildEnergy (AObject a) = W.childEnergy a

addIfWain
  :: Object a rt -> [PatternWain a rt]
    -> [PatternWain a rt]
addIfWain (PObject _ _) xs = xs
addIfWain (AObject a) xs = a:xs

objectToWain :: Object a rt -> PatternWain a rt
objectToWain (PObject _ _) = error "audio, not wain"
objectToWain (AObject a) = a
