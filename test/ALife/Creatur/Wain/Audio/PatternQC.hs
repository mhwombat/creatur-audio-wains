------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.PatternQC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Audio.PatternQC
  (
    test
  ) where

import ALife.Creatur.Wain.Audio.Pattern
import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI)
import ALife.Creatur.Wain.Util (unitInterval)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary UIDouble where
  arbitrary = doubleToUI <$> choose unitInterval

sizedArbAudio :: Int -> Gen Audio
sizedArbAudio n = do
  ps <- vectorOf n (choose audioValueRange)
  return . mkAudio $ map abs ps

instance Arbitrary Audio where
  arbitrary = sized sizedArbAudio

prop_audioDiff_can_be_0 :: Audio -> Property
prop_audioDiff_can_be_0 img = property $ audioDiff img img == 0

prop_audioDiff_btw_0_and_1 :: Audio -> Audio -> Property
prop_audioDiff_btw_0_and_1 i1 i2 = property $ 0 <= x && x <= 1
  where x = audioDiff i1 i2

prop_audioDiff_symmetric :: Audio -> Audio -> Property
prop_audioDiff_symmetric i1 i2 = property $
  audioDiff i1 i2 == audioDiff i2 i1

-- prop_audio_patterns_are_normalised :: Audio -> Property
-- prop_audio_patterns_are_normalised p = property $ normalised p

prop_audio_patterns_are_initially_valid :: Audio -> Property
prop_audio_patterns_are_initially_valid p = property $ valid p

-- prop_makeAudioSimilar_result_is_normalised
--   :: Audio -> UIDouble -> Audio -> Property
-- prop_makeAudioSimilar_result_is_normalised a r b
--   = property . normalised $ makeAudioSimilar a r b

prop_makeAudioSimilar_result_is_valid
  :: Audio -> UIDouble -> Audio -> Property
prop_makeAudioSimilar_result_is_valid a r b
  = property . valid $ makeAudioSimilar a r b

data TwoPatternsSameLength = TwoPatternsSameLength Audio Audio 
  deriving Show

sizedTwoPatternsSameLength :: Int -> Gen TwoPatternsSameLength
sizedTwoPatternsSameLength n = 
  TwoPatternsSameLength <$> sizedArbAudio n
                             <*> sizedArbAudio n

instance Arbitrary TwoPatternsSameLength where
  arbitrary = sized sizedTwoPatternsSameLength

prop_zero_adjustment_is_no_adjustment :: 
  TwoPatternsSameLength -> Property
prop_zero_adjustment_is_no_adjustment (TwoPatternsSameLength a b) =
  property $ audioDiff b b' < aTad
  where b' = makeAudioSimilar a 0 b
        aTad = 1e-10

prop_full_adjustment_gives_perfect_match :: 
  TwoPatternsSameLength -> Property
prop_full_adjustment_gives_perfect_match
  (TwoPatternsSameLength a b) = property $ audioDiff b' a < aTad
  where b' = makeAudioSimilar a 1 b
        aTad = 1e-10

prop_makeAudioSimilar_improves_similarity :: 
  TwoPatternsSameLength -> UIDouble -> Property
prop_makeAudioSimilar_improves_similarity (TwoPatternsSameLength a b) r
  = not (null as) && a /= b ==> d2 < d1
      where d1 = audioDiff a b
            d2 = audioDiff a b'
            b' = makeAudioSimilar a r b
            (Audio _ as) = a

-- normalised :: Audio -> Bool
-- normalised (Audio _ xs) = abs(norm xs - 1) < aTad || norm xs == 0
--   where aTad = 1e-10
        
equiv :: Audio -> Audio -> Bool
equiv a b = audioDiff a b <= aTad
  where aTad = 0.00001

test :: Test
test = testGroup "ALife.Creatur.Wain.Audio.PatternQC"
  [
    testProperty "prop_serialize_round_trippable - Audio"
      (prop_serialize_round_trippable :: Audio -> Property),
    testProperty "prop_genetic_round_trippable - Audio"
      (prop_genetic_round_trippable equiv :: Audio -> Property),
    testProperty "prop_diploid_identity - Audio"
      (prop_diploid_identity (==) :: Audio -> Property),
    testProperty "prop_audioDiff_can_be_0"
      prop_audioDiff_can_be_0,
    testProperty "prop_audioDiff_btw_0_and_1"
      prop_audioDiff_btw_0_and_1,
    testProperty "prop_audioDiff_symmetric"
      prop_audioDiff_symmetric,
    -- testProperty "prop_audio_patterns_are_normalised"
    --   prop_audio_patterns_are_normalised,
    testProperty "prop_audio_patterns_are_initially_valid"
      prop_audio_patterns_are_initially_valid,
    -- testProperty "prop_makeAudioSimilar_result_is_normalised"
    --   prop_makeAudioSimilar_result_is_normalised,
    testProperty "prop_makeAudioSimilar_result_is_valid"
      prop_makeAudioSimilar_result_is_valid,
    testProperty "prop_zero_adjustment_is_no_adjustment"
      prop_zero_adjustment_is_no_adjustment,
    testProperty "prop_full_adjustment_gives_perfect_match"
      prop_full_adjustment_gives_perfect_match,
    testProperty "prop_makeAudioSimilar_improves_similarity"
      prop_makeAudioSimilar_improves_similarity    
    
  ]
