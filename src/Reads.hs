module Reads
    ( defTblName
    , ngramSize
    , getHashes
    , hashCount
    , getNumEntries
    , getTblNames
    )
where

import BasicDB

import Text.Printf

import Database.HDBC
import Database.HDBC.Sqlite3


defTblName :: IO (String)
defTblName = do
    cnx <- getCnx
    results <- quickQuery' cnx "SELECT tblName FROM defaultTbl;" []
    disconnect cnx
    return $ (fromSql . head . concat) results

ngramSize :: IO (Int)
ngramSize = do
    cnx <- getCnx
    let query = "SELECT ngramSize FROM meta WHERE tblName=?;"
    tblName <- defTblName
    results <- quickQuery' cnx query [toSql tblName]
    disconnect cnx
    return $ (fromSql . head . concat) results

getHashes :: String -> IO ([Int])
getHashes fpath = do
    cnx <- getCnx
    tblName <- defTblName
    let query = printf "SELECT * FROM %s WHERE filePath=?;" tblName
    results <- quickQuery' cnx query [toSql fpath]
    disconnect cnx
    let hashes = tail $ fromSql <$> (concat results)
    return hashes

hashCount :: IO (Int)
hashCount = do
    cnx <- getCnx
    -- For some reason, using PRAGMA table_info does not return
    -- all columns.
    tblName <- defTblName
    let query = printf "SELECT hashCount FROM meta WHERE tblName=?;"
    results <- quickQuery' cnx query [toSql tblName]
    disconnect cnx
    return $ (fromSql . head . concat) results

getNumEntries :: String -> IO Int
getNumEntries tblName = do
    let query = printf "SELECT COUNT(*) FROM %s;" tblName
    cnx <- getCnx
    results <- quickQuery' cnx query []
    disconnect cnx
    return $ (fromSql . head . concat) results

getTblNames :: IO [String]
getTblNames = do
    cnx <- getCnx
    results <- quickQuery' cnx "SELECT tblName FROM meta;" []
    disconnect cnx
    return $ fromSql <$> concat results
