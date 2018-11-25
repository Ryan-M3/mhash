{-|
Module     : Groupijngs
Description: Helper functions related with grouping elements.
License    : MIT
Maintainer : Ryan McNamara <gn341ram@gmail.com>
Portability: Linux
|-}

{-# LANGUAGE QuasiQuotes #-}

module Groupings
    ( ngram
    , toGroupsOfN
    , getNumBuckets
    , tokenize
    ) where

import StopWords

import Data.List
import Data.Bits
import Data.Char
import Text.Regex.Posix

-- |Break a set of ordered items down into a set of all
-- consecutive elements of length n.
--
-- Example usage:
--   Input : print ngrams 3 [0..]
--   Output: "[[0, 1, 2], [1, 2, 3], [2, 3, 4], ..."
ngram :: Int -> [a] -> [[a]]
ngram n items
  | length items < n = []
  | otherwise        = [take n items] ++ ngram n (tail items)

tokenize :: String -> [String]
tokenize = filter (`notElem` stopWords) . words'
    where words' = getAllTextMatches . (=~ "\\w+") . (map toLower)

toGroupsOfN :: Int -> [a] -> [[a]]
toGroupsOfN n xs 
  | length xs <= n = [xs]
  | otherwise      = [take n xs] ++ toGroupsOfN n (drop n xs)

-- |Estimate the number of buckets needed do nearest-neighbor search
-- given some prefernce for performance, where 0% means accuracy is
-- the only thing that's important and 100% performance means all
-- you really care about is how quickly the query can be done.
getNumBuckets :: Int -> Int -> Float -> Int
getNumBuckets numHashes numEntries performance =
    let size = fromIntegral numEntries * fromIntegral numHashes * performance
     in min numHashes $ max 1 $ (round . log)  size
