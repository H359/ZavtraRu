{-# LANGUAGE OverloadedStrings #-}
module Gazeta where
-- encoding hackage failed to install :-/
--import Data.Encoding
--import Data.Encoding.CP1251
--import Data.Encoding.UTF8
import Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy as B

data Issue = Issue {
    pub_date :: Integer,
    articles :: [Article]
}

data Article = Article {
    title   :: B.ByteString,
    authors :: [Author],
    text    :: B.ByteString
}

data Author = Author {
    firstname :: B.ByteString,
    lastname  :: B.ByteString,
    username  :: B.ByteString
}

getTitle :: B.ByteString -> B.ByteString
getTitle str = B.empty

getAuthors :: B.ByteString -> [Author]
getAuthors str = []

getText :: B.ByteString -> B.ByteString
getText str = str

constructArticle :: B.ByteString -> Article
constructArticle charmesh = Article { title = title', authors = authors', text = text' }
    where charmesh8 = convert "CP1251" "UTF-8" charmesh
	  title' = getTitle charmesh8
	  authors' = getAuthors charmesh8
	  text' = getText charmesh8

showArticle :: Article -> IO ()
showArticle article = B.putStrLn (text article)