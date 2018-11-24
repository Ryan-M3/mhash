{-# LANGUAGE QuasiQuotes #-}

module Hash
    ( ngram
    , mhash
    , jaccard
    , toGroupsOfN
    , getNumBuckets
    , hash
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

-- |Hash text  into an 8-bit integer.  This is not a  very random
-- hash,  but the  minhash algorithm  actually works  better that
-- way.
hash_ :: Int -> String -> String -> Int
hash_ bits salt ""  = 255 -- use max int since any content trumps none
hash_ bits salt txt =
    let shiftNs = take  (length txt + 1) $ cycle [0..bits - 16]
        txt'    = salt ++ txt
     in foldl1 xor $ (shift . ord <$> txt') <*> shiftNs

hash = hash_ 64

-- |Find the minhash of a document using a single salt. This is a
-- helper function  to mhash generates salts  and applies minhash
-- to the document for you.
unsalted :: [String] -> String -> Int
unsalted shingles salt =
    let hashes = (hash salt) <$> shingles
     in if hashes == [] then minBound else minimum hashes

-- |The minhashing algorithm creates a locality-sensitive hash of
-- a document. A locality-sensitive hash  is a hash such that two
-- similar documents  will have more similar  hashes, measured by
-- Jaccard similarity.  Jaccard similarity  is the  proportion of
-- elements in two sets which are The same.
--
-- Example usage:
-- let txt     = "It was the best of times, it was the worst of..."
--     bigrams = ngram 2
--  in hashed = take 200 $ mhash bigrams txt
mhash :: ([String] -> [[String]])   -- ngram function (bigrams, 3-grams...)
      -> String                     -- full document to be minhashed
      -> [Int]                      -- an infinite list of hashes
mhash ngramFn txt =
    let shingles = concat <$> (ngramFn . tokenize $ txt)
     in (unsalted shingles) . show <$> [1..]

jaccard :: Eq a => [a] -> [a] -> Double
jaccard a b = numer / denom
    where similar = (==) <$> a <*> b
          numer = fromIntegral $ length $ filter (==True) similar
          denom = fromIntegral $ length similar

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
