#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where
import IO (stderr, hPutStrLn)
import Data.Either (lefts, rights)
import Data.Maybe (fromJust)
import Codec.Archive.Zip
import qualified System.Directory as S
import qualified System.FilePath.Posix as FP
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time.Calendar (fromGregorian, showGregorian, addDays)
import Control.Applicative ((<$>))
import Gazeta

addMeta :: Article -> String -> Article
addMeta a f = Article{
    articleUrl=f,
    articleText=articleText a,
    articleAuthors=articleAuthors a,
    articleTitle=articleTitle a,
    articlePubdate=articlePubdate a}

processArticle :: Archive -> C.ByteString -> Either C.ByteString Article
processArticle archive filepath = do
    case findEntryByPath filepath' archive of
        Nothing -> Left $ B.concat ["Archive operation failure in ", filepath]
        Just f  -> Right $ addMeta (parseArticle $ fromEntry f) filepath'
    where filepath' = C.unpack filepath

isHtml :: C.ByteString -> Bool
isHtml filename = html `B.isSuffixOf` filename
    where html = (".html"::C.ByteString)

isIndex :: C.ByteString -> Bool
isIndex filename = filename == indexFile
    where indexFile = ("index.html"::C.ByteString)

isJunk :: C.ByteString -> Bool
isJunk filename = (gst `B.isSuffixOf` filename) || (printH `B.isPrefixOf` filename)
    where gst = ("gst.html"::C.ByteString)
	  printH = ("print"::C.ByteString)

getIssueDate :: String -> String
getIssueDate filepath = deducedDate
    where deducedDate = showGregorian $ addDays diff start
	  filename = (FP.dropExtension . FP.takeFileName) filepath
	  issueNumber :: Integer
	  issueNumber = read filename
	  diff        = 7*(issueNumber - 150) -- days past 150-th issue
	  start       = fromGregorian 1996 10 15 -- 150 -> 15-10-96

insertAuthor author = putStrLn $ "<author><![CDATA[" ++ author ++ "]]></author>"

insertArticle :: Article -> IO ()
insertArticle article = if T.length text == 0 
    then return ()
    else ( do
        putStrLn $ "<article pubdate='" ++ (articlePubdate article) ++ "'>"
        putStrLn $ "<title><![CDATA[" ++ (articleTitle article) ++ "]]></title>"
        putStrLn $ "<url><![CDATA[" ++ (articleUrl article) ++ "]]></url>"
        putStrLn $ "<authors>"
        mapM_ insertAuthor (articleAuthors article)
        putStrLn $ "</authors>"
        putStrLn $ "<text><![CDATA[" ++ (T.unpack $ text) ++ "]]></text>"
        putStrLn $ "</article>"
    )
    where text = articleText article

fixArticle :: String -> Article -> Article
fixArticle d a = Article{articleUrl=articleUrl a, articleText = articleText a, articleAuthors = articleAuthors a, articleTitle = articleTitle a, articlePubdate = d}

getDefaultDate :: [String] -> String -> String
getDefaultDate p filename = if length p > 0 then head p else getIssueDate filename

processIssue archiveFile = do
    archive <- toArchive <$> B.readFile archiveFile
    let files     = filter (\s -> (isHtml s) && not ((isIndex s) || (isJunk s))) $ map (C.pack) (filesInArchive archive)
    let processed = map (processArticle archive) files
    let articles  = rights processed
    let errors    = lefts processed
    let defaultDate = getDefaultDate (filter (\s -> length s > 0) (map articlePubdate articles)) archiveFile
    let fixedArticles = map (fixArticle defaultDate) articles
    mapM_ (\s -> hPutStrLn stderr $ ("Parse error:" ++ (C.unpack s) ++ " in " ++ archiveFile)) errors
    mapM_ insertArticle fixedArticles

meaningFulDirectory s = s /= "." && s /= ".."

processYearDirectory directory = do
    issues <- S.getDirectoryContents directory
    let issueDirectories = map (\s -> concat [directory, "/", s]) $ filter meaningFulDirectory issues
    mapM_ processIssue issueDirectories

fetchIssues root = do
    years <- S.getDirectoryContents root
    let yearsDirectories = map (\s -> concat [root, "/", s]) $ filter meaningFulDirectory years
    mapM_ processYearDirectory yearsDirectories

main :: IO ()
main = do
    putStrLn "<?xml version='1.0' encoding='UTF-8'?><articles>"
    fetchIssues "/home/zw0rk/Work/zavtra_archive/data/zavtra"
    putStrLn "</articles>"