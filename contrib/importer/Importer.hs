#!/usr/bin/runghc
import Codec.Archive.Zip
import Data.List.Utils (join)
import System.Directory
import qualified Data.ByteString.Lazy as B
import Control.Applicative

analyze :: Archive -> FilePath -> IO ()
analyze archive filepath = do
    case findEntryByPath filepath archive of
        Nothing -> putStr "oops\n"
        Just f  -> putStr $ (eRelativePath f) ++ " " ++ (show $ eCompressedSize f) ++ " " ++ (show $ eUncompressedSize f) ++ "\n"

main :: IO ()
main = do
    archive <- toArchive <$> B.readFile "/home/zw0rk/Work/zavtra_archive/data/zavtra/00/318.zip"
    let files = filesInArchive archive
    mapM_ (analyze archive) files
    putStr "done :-D"