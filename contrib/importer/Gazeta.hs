{-# LANGUAGE OverloadedStrings #-}
module Gazeta where
-- encoding hackage failed to install :-/
--import Data.Encoding
--import Data.Encoding.CP1251
--import Data.Encoding.UTF8
import Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Parsec.Combinator --(manyTill)
import Text.Parsec.Char --(anyChar, string)
import Text.Parsec.Prim --(try, parse)
import Text.Parsec.Error
import Text.Parsec.ByteString.Lazy (Parser)

data Issue = Issue {
    pub_date :: Integer,
    articles :: [Article]
}

data Article = Article {
    title   :: C.ByteString,
    authors :: [Author],
    text    :: C.ByteString
}

data Author = Author {
    firstname :: C.ByteString,
    lastname  :: C.ByteString,
    username  :: C.ByteString
}

authorParser :: Parser C.ByteString
authorParser = do
    name <- many1 (noneOf ",")
    return (C.pack name)

authorsParser :: Parser C.ByteString
authorsParser = do
    string "Author: "
    authors <- many1 (noneOf "\n") --authorParser
    string "\n"
    return (C.pack authors)

titleParser :: Parser C.ByteString
titleParser = do
    string "Title: "
    title <- many (noneOf "\n")
    string "\n"
    return (C.pack title)

agaParser :: Parser Article
--C.ByteString
agaParser = do
    manyTill anyChar (try (string "<agahidd\n"))
    authors <- optional authorsParser
    title <- titleParser
    manyTill anyChar (try (string "endhidd>"))
    return Article { text="DONE", title=title, authors=[] }

getMetaArticle :: C.ByteString -> Article
getMetaArticle str = case parse agaParser "(unknown)" str of
    Left e  -> Article { title=errmsg , text="ERROR", authors=[] }
	where errmsg = C.pack $ concat (map messageString (errorMessages e))
    Right s -> s

getAuthors :: String -> [Author]
getAuthors str = []

getText :: String -> String
getText str = str

constructArticle :: C.ByteString -> Article
constructArticle charmesh =  getMetaArticle charmesh8
    where charmesh8 = convert "CP1251" "UTF-8" charmesh

showArticle :: Article -> IO ()
showArticle article = C.putStrLn $ C.concat [title article, " - ", text article]