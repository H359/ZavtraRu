#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Either (lefts, rights)
import Data.Maybe (fromJust)
import Codec.Archive.Zip
import qualified Data.List as L
import qualified System.Directory as S
import qualified System.FilePath.Posix as FP
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time.Calendar (fromGregorian, showGregorian, addDays)
--import qualified Data.Object as DO
--import qualified Data.Object.Yaml as DOY
import qualified Text.JSON as JSON
import Control.Applicative ((<$>))
import Gazeta

addMeta :: Article -> String -> String -> Article
addMeta a p f = Article{
    articleUrl=f,
    articleText=articleText a,
    articleAuthors=articleAuthors a,
    articleTitle=articleTitle a,
    articlePubdate=p}

processArticle :: Archive -> String -> C.ByteString -> Either C.ByteString Article
processArticle archive pubdate filepath = do
    case findEntryByPath filepath' archive of
        Nothing -> Left $ B.concat ["Archive operation failure in ", filepath]
        Just f  -> case parseArticle $ fromEntry f of
    	    Left  e -> Left $ B.concat [C.pack $ show e, " in ", filepath]
    	    Right s -> Right $ addMeta s pubdate filepath'
    where filepath' = C.unpack filepath

isHtml :: C.ByteString -> Bool
isHtml filename = html `B.isSuffixOf` filename
    where html = (".html"::C.ByteString)

isIndex :: C.ByteString -> Bool
isIndex filename = filename == indexFile
    where indexFile = ("index.html"::C.ByteString)

getIssueDate :: String -> String
getIssueDate filepath = showGregorian $ addDays diff start
    where filename = (FP.dropExtension . FP.takeFileName) filepath
	  issueNumber :: Integer
	  issueNumber = read filename
	  diff        = 7*(issueNumber - 150) -- days past 150-th issue
	  start       = fromGregorian 1996 10 15 -- 150 -> 15-10-96

processIssue :: String -> IO [Article]
processIssue archiveFile = do
    archive <- toArchive <$> B.readFile archiveFile
    let pubdate   = getIssueDate archiveFile
    let files     = filter (\s -> (isHtml s) && (not $ isIndex s)) $ map (C.pack) (filesInArchive archive)
    let processed = map (processArticle archive pubdate) files
    let articles  = rights processed
    let errors    = lefts processed
    mapM_ (\s -> putStrLn $ ("Parse error:" ++ (C.unpack s) ++ " in " ++ archiveFile)) errors
    return articles

meaningFulDirectory s = s /= "." && s /= ".."

processYearDirectory directory = do
    issues <- S.getDirectoryContents directory
    let issueDirectories = map (\s -> concat [directory, "/", s]) $ filter meaningFulDirectory issues
    issues <- mapM (\s -> processIssue s) issueDirectories
    return $ concat issues

fetchIssues :: String -> IO [Article]
fetchIssues root = do
    years <- S.getDirectoryContents root
    let yearsDirectories = map (\s -> concat [root, "/", s]) $ filter meaningFulDirectory years
    issues <- mapM processYearDirectory yearsDirectories
    return $ concat issues

instance JSON.JSON Article where 
    showJSON a = JSON.JSObject $ JSON.toJSObject [
	("title", JSON.showJSON $ articleTitle a),
	("authors", JSON.JSArray $ map (\s -> JSON.showJSON s) (articleAuthors a)),
	("url", JSON.showJSON $ articleUrl a),
	("pubdate", JSON.showJSON $ articlePubdate a),
	("text", JSON.showJSON $ T.unpack $ articleText a)]

main :: IO ()
main = do
    issues <- fetchIssues "/home/zw0rk/Work/zavtra_archive/data/zavtra"
    let encoded = JSON.encode issues
    putStrLn encoded