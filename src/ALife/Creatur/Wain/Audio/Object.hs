------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.Object
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with objects that could be either wains or
-- audio samples.
--
------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.Audio.Object
  (
    Object(..),
    isAudio,
    objectId,
    objectNum,
    objectAppearance,
    addIfWain,
    objectToWain
  ) where

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Audio.Pattern (Audio)
import ALife.Creatur.Wain.Audio.Tweaker (PatternTweaker(..))
import Control.Lens

data Object a = IObject Audio String
              | AObject (W.Wain Audio PatternTweaker a)

isAudio :: Object a -> Bool
isAudio (IObject _ _) = True
isAudio (AObject _) = False

objectId :: Object a -> String
objectId (IObject _ s) = "Audio " ++ s
objectId (AObject a) = agentId a

objectNum :: Object a -> Int
objectNum (IObject _ s) = read $ take 2 s
objectNum (AObject _) = 10

objectAppearance :: Object a -> Audio
objectAppearance (IObject img _) = img
objectAppearance (AObject a) = view W.appearance a

-- objectEnergy :: Object a -> UIDouble
-- objectEnergy (IObject _ _) = 0
-- objectEnergy (AObject a) = view W.energy a

-- objectChildEnergy :: Object a -> Double
-- objectChildEnergy (IObject _ _) = 0
-- objectChildEnergy (AObject a) = W.childEnergy a

addIfWain
  :: Object a -> [W.Wain Audio PatternTweaker a]
    -> [W.Wain Audio PatternTweaker a]
addIfWain (IObject _ _) xs = xs
addIfWain (AObject a) xs = a:xs

objectToWain :: Object a -> W.Wain Audio PatternTweaker a
objectToWain (IObject _ _) = error "audio, not wain"
objectToWain (AObject a) = a
