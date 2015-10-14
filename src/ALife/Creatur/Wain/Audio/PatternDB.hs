------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.PatternDB
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A ridiculously simple database that stores each record in a
-- separate file. The name of the file is the record's key.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Audio.PatternDB
  (
    PatternDB,
    mkPatternDB,
    anyPattern
  ) where

import ALife.Creatur.Wain.Audio.Pattern (Audio, readAudio)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, put)
import System.Directory (getDirectoryContents)
import System.Random (randomRIO)

-- | A simple database where each record is stored in a separate file, 
--   and the name of the file is the record's key.
data PatternDB = PatternDB
  {
    initialised :: Bool,
    mainDir :: FilePath,
    audios :: [FilePath],
    numPatterns :: Int,
    maxVectorCount :: Int
  } deriving (Eq)

instance Show PatternDB where
  show = mainDir

-- | @'mkPatternDB' d@ (re)creates the PatternDB in the
--   directory @d@.
mkPatternDB :: FilePath -> Int -> PatternDB
mkPatternDB d k = PatternDB False d undefined undefined k

initIfNeeded :: StateT PatternDB IO ()
initIfNeeded = do
  isInitialised <- gets initialised
  unless isInitialised $ do
    db <- get
    db' <- liftIO $ initialise db
    put db'

initialise :: PatternDB -> IO PatternDB
initialise db = do
  files <- liftIO . getDirectoryContents . mainDir $ db
  let audioFiles = filter isPatternFileName files
  return db { initialised=True, audios=audioFiles,
             numPatterns=length audioFiles }

isPatternFileName :: String -> Bool
isPatternFileName s =
  s `notElem` [ "archive", ".", ".." ]

anyPattern :: StateT PatternDB IO (Audio, String)
anyPattern = do
  initIfNeeded
  db <- get
  k <- liftIO $ randomRIO (0,numPatterns db - 1)
  let filename = audios db !! k
  img <- liftIO $
           readAudio (mainDir db ++ '/':filename) (maxVectorCount db)
  return (img, filename)
