------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.Audio.Util
-- Copyright   :  (c) Amy de Buitléir 2011-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.Audio.Util
  (
    stretch,
    formatVector
  ) where

import Data.List (intercalate)
import Text.Printf (printf)

-- | Stretches a vector (as needed) by duplicating some elements.
--
--   Examples:
--     λ> stretch "abcdefghi" 9
--     "abcdefghi"
--     λ> stretch "abcdefghi" 10
--     "abcdeefghi"
--     λ> stretch "abcdefghi" 11
--     "abcddefgghi"
--     λ> stretch "abcdefghi" 12
--     "abccdeefgghi"
--     λ> stretch "abcdefghi" 13
--     "abbcddeffghhi"
--     λ> stretch "abcdefghi" 14
--     "abbccddeffghhi"
stretch :: [a] -> Int -> Either String [a]
stretch xs n
  | null xs         = Left "can't stretch an empty string"
  | n < length xs   = Left "sequence too long" -- or, take n xs
  | n <= 2*length xs = Right (stretch' xs n)
  | otherwise       = stretch (double xs) n

stretch' :: [a] -> Int -> [a]
stretch' xs n = concat $ y:(map dupFirst ys)
  where dupCount = n - length xs
        (y:ys) = pieces (dupCount + 1) xs
        dupFirst (z:zs) = z:z:zs
        dupFirst [] = []

-- | Splits a list into the specified number of pieces. The last piece
--   may be shorter, depending on the input.
--   Note: If the length of the input list is not evenly divisible by n,
--   the pieces won't all be the same length. The shorter pieces will be
--   at the beginning of the resulting list. This is probably OK for
--   your needs, but it would be nicer to alternate long and short
--   pieces.
pieces :: Int -> [a] -> [[a]]
pieces n xs = if n > 0
                 then ys : (pieces (n-1) zs)
                 else []
  where (ys,zs) = splitAt k xs
        k = length xs `div` n


double :: [a] -> [a]
double [] = []
double (a:as) = a:a:double as

formatVector :: String -> [Double] -> String
formatVector fmt = intercalate " " . map (printf fmt)
