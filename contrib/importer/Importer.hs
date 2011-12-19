#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Maybe (fromJust)
import Codec.Archive.Zip
import qualified Data.List as L
import qualified System.Directory as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Applicative ((<$>))
import Gazeta

processArticle :: Archive -> C.ByteString -> Maybe Article
processArticle archive filepath = do
    case findEntryByPath (C.unpack filepath) archive of
        Nothing -> Nothing
        Just f  -> Just (constructArticle $ fromEntry f)

isHtml :: C.ByteString -> Bool
isHtml filename = html `B.isSuffixOf` filename
    where html = (".html"::C.ByteString)

index :: C.ByteString -> Bool
index filename = filename == indexFile
    where indexFile = ("index.html"::C.ByteString)

isArticle :: Maybe Article -> Bool
isArticle a = case a of
    Nothing -> False
    Just _  -> True

-- processIssue :: String -> Issue
processIssue :: C.ByteString -> IO Issue
processIssue archiveFile = do
    archive <- toArchive <$> B.readFile (C.unpack archiveFile)
    let files = filter (\f -> (not (index f)) && (isHtml f)) $ map (C.pack) (filesInArchive archive)
    --mapM_ (processArticle archive) files
    let articles = map fromJust $ filter isArticle $ map (processArticle archive) files
    return Issue{pub_date=0, articles=articles}

--main :: IO ()
--main = processIssue "/home/zw0rk/Work/zavtra_archive/data/zavtra/10/842.zip"

meaningFulDirectory s = s /= "." && s /= ".."

collectAuthors :: [Issue] -> [C.ByteString]
--
--collectAuthors issues = Set.toList issueAuthors
--	where
--	  issueAuthors = Set.fromList $ map (\s -> B.concat [firstname s, lastname s]) authorsList
--	  authorsList = concat $ map authors $ concat $ map articles issues
collectAuthors issues = L.sort $ L.nub issueAuthors
	where
	  issueAuthors = map (\s -> B.concat [firstname s, lastname s]) authorsList
	  authorsList = concatMap authors $ concatMap articles issues


withEnt :: C.ByteString -> C.ByteString
withEnt s = if C.count ' ' s > 3 then C.concat [s, " SUSPICIOUS"]
	    else s

processYearDirectory directory = do
    issues <- S.getDirectoryContents directory
    let issueDirectories = map (\s -> concat [directory, "/", s]) $ filter meaningFulDirectory issues
    issues <- mapM (\s -> processIssue (C.pack s)) issueDirectories
    mapM_ (\s -> C.putStrLn $ withEnt s) $ collectAuthors issues
    --C.putStrLn $ B.concat $ collectAuthors issues

main :: IO ()
main = do
    let root = "/home/zw0rk/Work/zavtra_archive/data/zavtra"
    years <- S.getDirectoryContents root
    let yearsDirectories = map (\s -> concat [root, "/", s]) $ filter meaningFulDirectory years
    mapM_ processYearDirectory yearsDirectories