module Services
    ( pprintSimilar
    , findSimilar
    , addDoc
    , setupDB
    , mkTbl
    , renameTblService
    , addDocsFromPath
    , listTbls
    , listDocs
    , mostSimilar
    ) where

import Groupings
import ParallelHash
import Reads
import Updates
import BasicDB
import MoonLogic

import System.IO
import Data.List
import Text.Printf
import Control.Monad

import System.Directory


pprintSimilar :: FilePath -> Int -> IO ()
pprintSimilar fpath n = do
    found <- findSimilar fpath
    let top10 = map fst $ take n found
    samples <- mapM getSample top10
    let titles = (printf "Path: %s \n") <$> top10
    putStrLn "Original:"
    putStrLn =<< hGetContents =<< openFile fpath ReadMode
    putStrLn "\n"
    putStrLn "Closest Matches:\n"
    putStrLn $ ("\n\n" ++) $ concat $ zipWith (++) titles samples

findSimilar :: FilePath -> IO ([(String, Double)])
findSimilar fpath = do
    hash       <- pHashDoc fpath
    tbl        <- defTblName
    numEntries <- getNumEntries tbl
    -- This big series of let statements are just for building up to a Sql
    -- statement that divides the search up into buckets and makes comparisons
    -- on that basis. 
    let numHashes   = digits hash 
    let colEqs    = printf "hash%s=?" <$> show <$> [1..numHashes] :: [String]
    let nbuckets  = getNumBuckets numHashes numEntries 0.75
    let colGroups = toGroupsOfN (max 1 $ numHashes `div` nbuckets) colEqs
    let intoBands = (intercalate " AND ") <$> colGroups
    let whereCond = intercalate ") OR (" intoBands
    let query = printf "SELECT filePath FROM %s WHERE (%s);" tbl whereCond
    cnx     <- getCnx
    results <- quickQuery' cnx query $ toSql <$> fromMoonInt hash
    disconnect cnx
    let similarPaths = fromSql <$> (concat results)
    simHashes <- mapM getHashes similarPaths
    let similarities = jaccard hash <$> simHashes
    return $ sortOn (negate . snd) $ zip similarPaths similarities

mostSimilar :: FilePath -> Int -> IO [String]
mostSimilar fpath n = do
    found <- findSimilar fpath
    return $ fst <$> (take n found)

getSample :: FilePath -> IO (String)
getSample filePath = do
    txt <- hGetContents =<< openFile filePath ReadMode
    return $ take 140 txt

addDoc :: FilePath -> IO ()
addDoc fpath = do
    txt <- hGetContents =<< openFile fpath ReadMode
    n   <- ngramSize
    if   (length . tokenize) txt < n
    then putStrLn $ "skipping " ++ fpath
    else pHashDoc fpath >>= addHashes fpath

addDoc' :: Int -> Int -> FilePath -> IO ()
addDoc' n hCount fpath = do
    txt <- hGetContents =<< openFile fpath ReadMode
    if   (length . tokenize) txt < n
    then putStrLn $ "skipping:\n\t" ++ txt
    else pHashDoc' n hCount txt >>= addHashes fpath

setupDB :: IO ()
setupDB = mkDefTbl >> mkMetaTbl

mkTbl :: String -> Int -> Int -> IO ()
mkTbl tblName nhashes ngramSize = do
    mkNewTbl  tblName nhashes
    addToMeta tblName ngramSize nhashes

renameTblService :: String -> String -> IO ()
renameTblService old new = renameTbl old new >> renameTblMeta old new

addDocsFromPath :: FilePath -> IO ()
addDocsFromPath dirPath = do
    files  <- listDirectory dirPath
    let fullFilePaths = (dirPath++) <$> files
    mapM_ addDoc fullFilePaths

listTbls :: IO ()
listTbls = getTblNames >>= print

listDocs :: String -> IO ()
listDocs "" = defTblName >>= listDocs
listDocs tblName = listPaths tblName >>= print'
    where print' = \txt -> print $ intercalate "\n" txt
