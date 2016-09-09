------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Analyse a wain and generate a report.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.Audio.ExamineAgent where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.Brain
import ALife.Creatur.Wain.GeneticSOM
import ALife.Creatur.Wain.Response (Response)
import ALife.Creatur.Wain.Audio.Wain
import ALife.Creatur.Wain.UnitInterval
import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Serialize as DS
import System.Directory (getDirectoryContents)
import System.Posix (isDirectory)
import System.Posix.Files (getFileStatus)
import Text.Printf (printf)

fetchWains
  :: (DS.Serialize a, DS.Serialize rt, Ord a, Tweaker rt,
    Response a ~ Pattern rt)
      => FilePath -> IO [PatternWain a rt]
fetchWains f = do
  dir <- isDirectory <$> getFileStatus f
  if dir
    then fetchAllWains f
    else do
      w <- fetchWain f
      return [w]

fetchAllWains
  :: (DS.Serialize a, DS.Serialize rt, Ord a, Tweaker rt,
    Response a ~ Pattern rt)
      => FilePath -> IO [PatternWain a rt]
fetchAllWains f = do
  fs <- drop 2 <$> getDirectoryContents f
  mapM fetchWain fs

fetchWain
  :: (DS.Serialize a, DS.Serialize rt, Ord a, Tweaker rt,
    Response a ~ Pattern rt)
      => FilePath -> IO (PatternWain a rt)
fetchWain f = do
  x <- BS.readFile f
  let (Right w) = DS.decode x
  return w

examine :: Show a => PatternWain a rt -> IO ()
examine a = do
  putStrLn $ "name: " ++ show (view name a)
  -- appearance
  -- brain
  putStrLn $ "devotion: " ++ printf "%5.3f" (uiToDouble $ view devotion a)
  putStrLn $ "ageOfMaturity: " ++ show (view ageOfMaturity a)
  putStrLn $ "passionDelta: " ++ show (view passionDelta a)
  putStrLn $ "energy: " ++ printf "%5.3f" (uiToDouble $ view energy a)
  putStrLn $ "passion: " ++ printf "%5.3f" (uiToDouble $ view passion a)
  putStrLn $ "age: " ++ show (view age a)
  putStrLn $ "total # children borne: "
    ++ show (view childrenBorneLifetime a)
  putStrLn $ "total # children weaned: "
    ++ show (view childrenWeanedLifetime a)
  putStrLn $ "litter size: " ++ show (length . view litter $ a)
  putStrLn $ "classifier SQ: "
    ++ show (schemaQuality . view classifier . view brain $ a)
  putStrLn $ "predictor SQ: "
    ++ show (schemaQuality . view predictor . view brain $ a)
  putStrLn $ "DSQ: " ++ show (decisionQuality . view brain $ a)
  putStrLn $ "Number of classifier models: "
    ++ show (numModels . view classifier . view brain $ a)
  putStrLn $ "Classifier learning function "
    ++ show (view learningParams . view classifier . view brain $ a)
  putStrLn $ "Classifier counts: "
    ++ show (counterMap . view classifier . view brain $ a)
  mapM_ putStrLn $ describeClassifierModels a
  putStrLn $ "Number of predictor models: "
    ++ show (numModels . view predictor . view brain $ a)
  putStrLn $ "Predictor learning function "
    ++ show (view learningParams . view predictor . view brain $ a)
  putStrLn $ "Predictor counts: "
    ++ show (counterMap . view predictor . view brain $ a)
  mapM_ putStrLn $ describePredictorModels a
  -- putStrLn "--------"
  -- putStrLn "Raw data"
  -- putStrLn "--------"
  -- putStrLn $ show a

formatVector :: String -> [Double] -> String
formatVector fmt = unwords . map (printf fmt)
