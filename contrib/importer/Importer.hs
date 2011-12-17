#!/usr/bin/runghc
import Codec.Archive.Zip
import Data.List.Utils (join)
import System.Directory
import qualified Data.ByteString.Lazy as B
import Control.Applicative

analyze :: FilePath -> IO ()
analyze filepath = do
    putStr $ filepath ++ "\n"

main :: IO ()
main = do
    archive <- toArchive <$> B.readFile "/home/zw0rk/Work/zavtra_archive/data/zavtra/00/318.zip"
    let files = filesInArchive archive
    mapM_ analyze files
    putStr "done :-D"