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

trim :: C.ByteString -> C.ByteString
trim s = (ltrim . rtrim)  s

ltrim :: C.ByteString -> C.ByteString
ltrim s = C.dropWhile (\c -> c == ' ') s

rtrim :: C.ByteString -> C.ByteString
rtrim s = C.reverse $ ltrim $ C.reverse s

eol :: Parser Char
eol = char '\r' <|> char '\n'

authorParser :: Parser Author
--C.ByteString
authorParser = do
    name <- many1 (noneOf [',', ':', '\r', '\n', '—'])
    many (char ',' <|> char ':' <|> char '—')
    return Author{firstname=trim $ C.pack name,lastname="",username=""}

authorsParser :: Parser [Author]
authorsParser = do
    string "Author: "
    authors <- many1 authorParser
    return authors
    --authors <- many1 (noneOf ['\r','\n']) --authorParser
    --return (C.pack authors)

titleParser :: Parser C.ByteString
titleParser = do
    string "Title: "
    title <- many (noneOf ['\n','\r'])
    return (C.pack title)

mkAuthors a = case a of
    Just s  -> s
    Nothing -> []

agaParser :: Parser Article
--C.ByteString
agaParser = do
    manyTill anyChar (try (string "<agahidd"))
    many eol
    authors <- optionMaybe authorsParser
    many eol
    title <- titleParser
    manyTill anyChar (try (string "endhidd>"))
    return Article { text = "DONE", title = title, authors = mkAuthors authors }

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
showArticle article = C.putStrLn $ C.concat ["\t\t", title', " - ", text', " - ", authors']
    where title'   = title article
	  text'    = text article
 	  authors' = C.concat $ map (\p -> C.concat ["[", firstname p, "]"]) $ authors article