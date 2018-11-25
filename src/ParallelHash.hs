module ParallelHash where

import Groupings (tokenize, ngram)
import Reads
import MoonLogic

import Data.List
import Data.Bits
import Data.Char
import System.IO

import Control.Parallel
import Control.Parallel.Strategies
import Text.Regex.Posix

{-      hash0   hash1   hash2   hash3 
   txt0 06251   15699   74161   15611 <- produced by hashShingle
   txt1 16161   00185   16984   91812
   txt2 51951   59119   59100   01101
   txt3 09920   23444   99121   21101
   ...  ...     ...     ...     ...
        -----   -----   -----   -----
        minH0   minH1   minH2   minH3 <- produced after all rows are processed
-}

-- |Minhash without the bitsize specified.
hash' :: Int -> String -> String -> Int
hash' bits salt ""  = maxBound -- any text trumps no text
hash' bits salt txt =
    let shiftNs = take (length txt + 1) $ cycle [0..bits - 16]
        txt'    = salt ++ txt
     in foldl1 xor $ (shift . ord <$> txt') <*> shiftNs

-- |Hash a single string given as salt string.
hash = hash' 64

hashShingle :: Int -> String -> MoonInt
hashShingle nhashes txt = MoonInt m
    where m = zipWith hash (show <$> [1..nhashes]) (repeat txt) :: [Int]

mhash :: Int -> [String] -> MoonInt
mhash ngramSize shingles = mconcat moonInts
    where moonInts = (parMap rseq) (hashShingle ngramSize) shingles

pHashDoc :: FilePath -> IO (MoonInt)
pHashDoc fpath = do
    txt    <- hGetContents =<< openFile fpath ReadMode
    n      <- ngramSize
    hcount <- hashCount
    case length txt of
      0 -> error "Can't hash empty document."
      _ -> return $ mhash hcount $ concat $ ngram n $ tokenize txt
