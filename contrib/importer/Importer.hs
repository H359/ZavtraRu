#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Either (lefts, rights)
import Codec.Archive.Zip
import qualified Data.List as L
import qualified System.Directory as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative ((<$>))
import Gazeta

processArticle :: Archive -> C.ByteString -> Either C.ByteString Article
processArticle archive filepath = do
    case findEntryByPath (C.unpack filepath) archive of
        Nothing -> Left $ B.concat ["Archive operation failure in ", filepath]
        Just f  -> case (parseArticle $ fromEntry f) of
    	    Left  e -> Left $ B.concat [C.pack $ show e, " in ", filepath]
    	    Right s -> Right s

isHtml :: C.ByteString -> Bool
isHtml filename = html `B.isSuffixOf` filename
    where html = (".html"::C.ByteString)

index :: C.ByteString -> Bool
index filename = filename == indexFile
    where indexFile = ("index.html"::C.ByteString)

processIssue :: String -> IO Issue
processIssue archiveFile = do
    archive <- toArchive <$> B.readFile archiveFile
    let files     = filter (\f -> (not (index f)) && (isHtml f)) $ map (C.pack) (filesInArchive archive)
    let processed = map (processArticle archive) files
    let articles  = rights processed
    let errors    = lefts processed
    --mapM_ (\s -> putStrLn $ ("Parse error:" ++ (C.unpack s) ++ " in " ++ archiveFile)) errors
    return Issue{pub_date=0, articles=articles}

meaningFulDirectory s = s /= "." && s /= ".."

collectAuthors :: [Issue] -> [T.Text]
collectAuthors issues = issueAuthors
	where
	  issueAuthors = map firstname authorsList
	  authorsList = concatMap authors $ concatMap articles issues

processYearDirectory directory = do
    issues <- S.getDirectoryContents directory
    let issueDirectories = map (\s -> concat [directory, "/", s]) $ filter meaningFulDirectory issues
    issues <- mapM (\s -> processIssue s) issueDirectories
    --mapM_ TIO.putStrLn (L.sort $ L.nub $ collectAuthors issues)
    return issues

main :: IO ()
main = do
    let root = "/home/zw0rk/Work/zavtra_archive/data/zavtra"
    years <- S.getDirectoryContents root
    let yearsDirectories = map (\s -> concat [root, "/", s]) $ filter meaningFulDirectory years
    issues <- mapM processYearDirectory yearsDirectories
    mapM_ TIO.putStrLn (L.sort $ L.nub  $ collectAuthors $ concat issues)
    TIO.putStrLn "OK"