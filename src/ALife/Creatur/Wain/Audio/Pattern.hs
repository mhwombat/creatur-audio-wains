------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.Pattern
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Audio utilities.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Audio.Pattern
  (
    Audio(..),
    mkAudio,
    audioDiff,
    makeAudioSimilar,
    audioValueRange,
    randomAudio,
    readAudio,
    valid,
    invalid,
    norm,
    toRawData,
    selectFrames -- exported for testing
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic, put, get,
  Reader, putRawWord8, getRawWord8)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Audio.Util (stretch)
-- import ALife.Creatur.Wain.Pretty (Pretty, pretty)
import ALife.Creatur.Wain.UnitInterval (UIDouble, doubleToUI,
  uiToDouble)
import ALife.Creatur.Wain.Util (inRange)
import Control.Monad (replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandomRs)
import Data.Datamining.Pattern (adjustVector, euclideanDistanceSquared)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Serialize (Serialize)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)

-- | The range within which each number of an audio sample is
--   guaranteed to fall.
audioValueRange :: (Double,Double)
audioValueRange = (-100,100)

maxAudioValueDiffSquared :: Double
maxAudioValueDiffSquared = (b-a)*(b-a)
  where (b,a) = audioValueRange

audioValueToWord8 :: Double -> Word8
audioValueToWord8 x = round x + 100

word8ToAudioValue :: Word8 -> Double
word8ToAudioValue g = (fromIntegral g) - 100

data Audio = Audio Int [Double]
  deriving (Eq, Show, Generic)

mkAudio :: [Double] -> Audio
mkAudio xs = Audio (length xs) xs

toRawData :: Audio -> [Double]
toRawData (Audio _ xs) = xs

instance Serialize Audio
instance Diploid Audio

instance Genetic Audio where
  put (Audio n xs) =
    put (fromIntegral n :: Word16) >>
    -- mapM_ put (map audioValueToWord8 xs)
    mapM_ putRawWord8 (map audioValueToWord8 xs)
  get = do
    header <- get :: Reader (Either [String] Word16)
    case header of
      Left s -> return $ Left s
      Right n -> do
        -- xs <- replicateM (fromIntegral n) get :: Reader [Either [String] Word8]
        xs <- replicateM (fromIntegral n) getRawWord8 :: Reader [Either [String] Word8]
        let ys = sequence xs :: Either [String] [Word8]
        return $ fmap (mkAudio . map word8ToAudioValue) ys

audioDiff :: Audio -> Audio -> UIDouble
audioDiff (Audio n xs) (Audio m ys)
    | n /= m     = error "length mismatch between audio patterns"
    | n == 0    = 0
    | otherwise = doubleToUI $ (euclideanDistanceSquared xs ys)/z
    where z = fromIntegral n * maxAudioValueDiffSquared
    -- scaled so that the difference is betwen 0 and 1.

makeAudioSimilar :: Audio -> UIDouble -> Audio -> Audio
makeAudioSimilar (Audio n xs) r (Audio m ys) =
    if n == m
      then mkAudio $ adjustVector xs (uiToDouble r) ys
      else error "length mismatch between audio patterns"

-- Used for generating the initial brain models in the initial
-- population.
-- | Generates an audio vector of the specified length.
randomAudio :: RandomGen r => Int -> Rand r Audio
randomAudio n
  = fmap (Audio n . take n) (getRandomRs audioValueRange)

readAudio :: FilePath -> Int -> IO Audio
readAudio filePath desiredVectorCount = do
  raw <- readFile filePath
  -- Each line in the text file contains one vector
  let vectors = map stringToVector $ lines raw
  let result = selectFrames vectors desiredVectorCount
  case result of
    Left msg     -> error $ "Problem with " ++ filePath ++ ": " ++ msg
    Right sample -> return . mkAudio $ concat sample

stringToVector :: String -> [Double]
stringToVector s = map read $ words s

valid :: Audio -> Bool
valid (Audio _ xs) = all (inRange audioValueRange) xs

invalid :: Audio -> Bool
invalid = not . valid

norm :: Floating a => [a] -> a
norm xs = sqrt $ sum (map f xs)
  where f x = x*x

selectFrames :: [[Double]] -> Int -> Either String [[Double]]
selectFrames xs n
  | l < n     = stretch xs n
  | l > n     = dropSmallestChanges xs (l-n)
  | otherwise = Right xs
  where l = length xs

dropSmallestChanges :: [[Double]] -> Int -> Either String [[Double]]
dropSmallestChanges (x:xs) n = Right (x:xs')
  where xs' = map snd . dropSmallestChanges' n $ dxs
        dxs = diffs (x:xs)
dropSmallestChanges [] _ = Left "zero-length vector"

dropSmallestChanges' :: Int -> [(Double, [Double])] -> [(Double, [Double])]
dropSmallestChanges' 0 xs = xs
dropSmallestChanges' n xs = dropSmallestChanges' (n-1) xs'
  where x = minimumBy (comparing fst) xs
        xs' = dropFirstOccurrence x xs
  
  
diffs :: [[Double]] -> [(Double, [Double])]
diffs (v1:v2:vs) = (euclideanDistanceSquared v1 v2, v2) : diffs (v2:vs)
diffs [_] = []
diffs [] = []

dropFirstOccurrence :: Eq a => a -> [a] -> [a]
dropFirstOccurrence x (y:ys)
  = if y == x then ys else y : dropFirstOccurrence x ys
dropFirstOccurrence _ [] = []

-- wombat [[0.1,0.2,0.3],[0.1,0.2,0.3],[0.2,0.4,0.6],[0.4,0.6,0.8],[0.2,0.4,0.6],[0.1,0.2,0.3],[0.1,0.2,0.3]]
-- [(0.0,[0.1,0.2,0.3]),(0.14,[0.2,0.4,0.6]),(0.12000000000000002,[0.4,0.6,0.8]),(0.12000000000000002,[0.2,0.4,0.6]),(0.14,[0.1,0.2,0.3]),(0.0,[0.1,0.2,0.3])]
