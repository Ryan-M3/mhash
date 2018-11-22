module BasicDB
    ( getCnx
    , execSql
    , module Database.HDBC
    , module Database.HDBC.Sqlite3
    )
where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory

getCnx :: IO (Connection)
getCnx = do
    appDir <- getAppUserDataDirectory "mhash"
    cnx    <- connectSqlite3 $ appDir ++ "/mhash.db"
    return cnx

execSql :: String -> [SqlValue] -> IO ()
execSql sql args = do
    cnx  <- getCnx
    stmt <- prepare cnx sql
    execute stmt args
    commit cnx
    disconnect cnx
