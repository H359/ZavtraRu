#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Codec.Archive.Zip
-- import Data.List.Utils (join)
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative
import Gazeta

processArticle :: Archive -> FilePath -> IO ()
processArticle archive filepath = do
    case findEntryByPath filepath archive of
        Nothing -> putStr "oops\n"
        Just f  -> do 
    	    let article = constructArticle $ fromEntry f
    	    showArticle article

isHtml :: FilePath -> Bool
isHtml filename = ".html" `B.isSuffixOf` (C.pack filename)

-- processIssue :: String -> Issue
processIssue :: String -> IO ()
processIssue archiveFile = do
    archive <- toArchive <$> B.readFile archiveFile
    let files = filter isHtml $ filesInArchive archive
    mapM_ (processArticle archive) files

main :: IO ()
main = processIssue "/home/zw0rk/Work/zavtra_archive/data/zavtra/00/318.zip"