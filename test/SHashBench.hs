{-|
Module     : SHashBench
Description: Benchmarking for the serial version of minhash.
License    : CC0
Maintainer : Ryan McNamara <gn341ram@gmail.com>
Portability: Linux

Contains now depricated functions for performing hashes in serial.
|-}

module SHashBench where

import StopWords
import Groupings
import PHashBench (docs)
import Reads

import Data.List
import Data.Bits
import Data.Char
import Text.Regex.Posix
import System.IO

tokenize' :: String -> [String]
tokenize' = filter (`notElem` stopWords) . words'
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
     in if hashes == [] then maxBound else minimum hashes

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
mhash ngramFn txt = let shingles = concat <$> (ngramFn . tokenize' $ txt)
     in (unsalted shingles) . show <$> [1..]
sHashDoc :: FilePath -> IO [Int]
sHashDoc fpath = do
    txt    <- hGetContents =<< openFile fpath ReadMode
    n      <- ngramSize
    hcount <- hashCount
    case length txt of
      0 -> error "Can't hash empty document."
      _ -> return $ take hcount $ mhash (ngram n) txt

sHashDocs :: Int -> IO ()
sHashDocs numDocs = do
    mapM_ sHashDoc (docs numDocs)
