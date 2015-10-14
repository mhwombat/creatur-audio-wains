------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.Audio.PatternQC (test)

import Test.Framework as TF (defaultMain, Test)

tests :: [TF.Test]
tests = 
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.Audio.PatternQC.test
  ]

main :: IO ()
main = defaultMain tests
