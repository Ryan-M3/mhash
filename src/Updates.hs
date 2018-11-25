module Updates
    ( addHashes
    , mkDefTbl
    , mkMetaTbl
    , mkNewTbl
    , addToMeta
    , setDefTbl
    , renameTbl
    , renameTblMeta
    , listPaths
    , delTbl
    ) where

import BasicDB
import Reads
import Groupings
import MoonLogic

import Text.Printf
import Data.List
import System.IO

addHashes :: FilePath -> MoonInt -> IO ()
addHashes _ (MoonInt []) = error "Received empty list of hashes."
addHashes fpath hashes = do
    tbl <- defTblName
    let qmarks = intercalate ", " $ replicate (digits hashes) "?"
    let query  = printf "INSERT OR REPLACE INTO %s VALUES (?, %s);" tbl qmarks
    let args   = [toSql fpath] ++ (toSql <$> (fromMoonInt hashes))
    execSql query args

mkDefTbl :: IO ()
mkDefTbl = execSql "CREATE TABLE IF NOT EXISTS defaultTbl (tblName STRING);" []

mkMetaTbl :: IO ()
mkMetaTbl = execSql cmd []
    where cmd = "CREATE TABLE IF NOT EXISTS meta " ++
                "(tblName STRING, ngramSize INT, hashCount INT);"

mkNewTbl :: String -> Int -> IO ()
mkNewTbl tblName ncols = execSql cmd []
    where cmd    = printf cmdTxt tblName (cols :: String)
          cmdTxt = "CREATE TABLE IF NOT EXISTS %s " ++
                   "(filePath STRING PRIMARY KEY, %s);"
          cols   = intercalate ", " $ map (printf "hash%d INTEGER") [1..ncols]

addToMeta :: String -> Int -> Int -> IO ()
addToMeta tblName ngramSize hashCount = do
    let cmd = "INSERT OR REPLACE INTO meta " ++
              "(tblName, ngramSize, hashCount) VALUES (?, ?, ?);"
     in execSql cmd [toSql tblName, toSql ngramSize, toSql hashCount]

setDefTbl :: String -> IO ()
setDefTbl tblName = do
    execSql "DELETE FROM defaultTbl;" []
    let cmd = "INSERT INTO defaultTbl (tblName) VALUES (?);"
     in execSql cmd [toSql tblName]

renameTbl :: String -> String -> IO ()
renameTbl oldName newName = execSql cmd []
    where cmd = printf "ALTER TABLE %s RENAME TO %s;" oldName newName

renameTblMeta :: String -> String -> IO ()
renameTblMeta oldName newName = execSql cmd [toSql oldName, toSql newName]
    where cmd = "UPDATE meta SET tblName='?' WHERE tblName='?';"

listPaths :: String -> IO [String]
listPaths tblName = do
    cnx <- getCnx
    let query = printf "SELECT filePath FROM %s;" tblName
    results <- quickQuery' cnx query []
    return $ fromSql <$> concat results

delTbl :: String -> IO ()
delTbl tblName = execSql ("DROP TABLE " ++ tblName ++ ";") []
