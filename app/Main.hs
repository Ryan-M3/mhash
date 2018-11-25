{-|
Module     : Main
Description: Dispatch terminal arguments to functions.
License    : MIT
Maintainer : Ryan McNamara <gn341ram@gmail.com>
Portability: Linux

Takes arguments form the terminal and calls the
appropriate function given those arguments.
|-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lib

import Control.Monad
import System.Environment
import System.IO
import Text.RawString.QQ

helpTxt :: String
helpTxt = [r|
Do first time set-up:
    $ mhash setup

Create a new table:
    $ mhash new <table name> <number of hashes> <ngram size>

Set a table as the default:
    $ mhash set-default <table name>
OR
    $ mhash set <table name>

Add new document:
    $ mhash add <path>

Add every file in a directory to the default table:
    $ mhash add-dir <file path>

Print n similar documents:
    $ mhash print-similar <path> <n>

Print all tables in the databse:
    $ mhash list-tables

Rename a table:
    $ mhash rename <old name> <new name>

Remove a table from the database (be careful!):
    $ mhash del <table nametblName>

Print the filepaths of all the documents in the default table:
    $ mhash list-docs

Print the filepaths of all the documents in a specified table:
    $ mhash list-docs <table name>

Find the n most similar documents to the
one specified by the file path:
    $ mhash find-similar <filepath> <n>

Display the very text you're reading now:
    $ mhash help
OR
    $ mhash -h
OR
    $ mhash ?
|]

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["add", fpath]              -> addDoc fpath
      ["print-similar", fpath, n] -> pprintSimilar fpath $ read n
      ["setup"]                   -> setupDB
      ["new", tblName, hashes, n] -> mkTbl tblName (read hashes) (read n)
      ["set-default", tblName]    -> setDefTbl tblName
      ["set"        , tblName]    -> setDefTbl tblName
      ["rename", old, new]        -> renameTblService old new
      ["add-dir", fpath]          -> addDocsFromPath fpath
      ["list-tables"]             -> listTbls
      ["list-docs"]               -> listDocs ""
      ["list-docs", tblName]      -> listDocs tblName
      ["find-similar", fpath, n]  -> mostSimilar fpath (read n) >>= mapM_ print 
      ["help"]                    -> putStrLn helpTxt
      ["-h"]                      -> putStrLn helpTxt
      ["?"]                       -> putStrLn helpTxt
      ["del", tblName]            -> delTbl tblName
      otherwise -> error "Invalid command line arguments. Try mhash help for commands."
    return ()
