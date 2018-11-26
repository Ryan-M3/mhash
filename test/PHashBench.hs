module PHashBench where

import Reads
import Updates
import BasicDB
import ParallelHash
import MoonLogic
import Groupings

import System.IO
import Text.Printf

import System.Directory

docs :: Int -> [String]
docs n = (printf fpath) <$> ([0..n-1] :: [Int])
    where fpath = "/home/ryan/Downloads/reddit_comments/%d.txt"

pHashDoc' :: FilePath -> IO (MoonInt)
pHashDoc' fpath = do
    txt    <- hGetContents =<< openFile fpath ReadMode
    n      <- ngramSize
    hcount <- hashCount
    case length txt of
      0 -> error "Can't hash empty document."
      _ -> return $ parMinHash hcount $ concat $ ngram n $ tokenize txt

pHashDocs' :: Int -> IO ()
pHashDocs' numDocs = do
    mapM_ pHashDoc (docs numDocs)
