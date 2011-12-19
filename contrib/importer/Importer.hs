#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Codec.Archive.Zip
--import System.Directory (FilePath)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative ((<$>))
import Gazeta

processArticle :: Archive -> C.ByteString -> IO ()
processArticle archive filepath = do
    case findEntryByPath (C.unpack filepath) archive of
        Nothing -> putStr "oops\n"
        Just f  -> do 
    	    let article = constructArticle $ fromEntry f
    	    showArticle article

isHtml :: C.ByteString -> Bool
isHtml filename = html `B.isSuffixOf` filename
    where html = (".html"::C.ByteString)

-- processIssue :: String -> Issue
processIssue :: C.ByteString -> IO ()
processIssue archiveFile = do
    archive <- toArchive <$> B.readFile (C.unpack archiveFile)
    let files = filter isHtml $ map (C.pack) (filesInArchive archive)
    mapM_ (processArticle archive) files

main :: IO ()
main = processIssue "/home/zw0rk/Work/zavtra_archive/data/zavtra/00/318.zip"