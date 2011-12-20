{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta where
-- encoding hackage failed to install :-/
--import Data.Encoding
--import Data.Encoding.CP1251
--import Data.Encoding.UTF8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy as BL
import Data.ByteString as B
import qualified Codec.Text.IConv as IConv
--import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim 
import Text.Parsec.Error

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons

type Parser = Parsec T.Text ()
type GenParser t st = Parsec T.Text st

data Issue = Issue {
    pub_date :: Integer,
    articles :: [Article]
}

data Article = Article {
    title   :: T.Text,
    authors :: [Author],
    text    :: T.Text
}

data Author = Author {
    firstname :: T.Text,
    lastname  :: T.Text,
    username  :: T.Text
}

eol = many (oneOf "\r\n")

authorParser :: Parser Author
authorParser = do
    name <- many1 (noneOf ",:\r\n\8212\8213")
    many (oneOf ":,\8212\8213")
    return Author{firstname = T.pack name,lastname = T.pack "", username = T.pack ""}

authorsParser :: Parser [Author]
authorsParser = do
    string "Author: "
    authors <- many1 authorParser
    return authors

titleParser :: Parser String
titleParser = do
    string "Title: "
    title <- many (noneOf "\r\n") 
    return title

mkAuthors a = case a of
    Just s  -> s
    Nothing -> []

agaParser :: Parser Article
agaParser = do
    manyTill anyChar (try (string "<agahidd"))
    eol
    authors <- optionMaybe authorsParser
    eol
    title <- titleParser
    manyTill anyChar (try (string "endhidd>"))
    return Article { text = T.pack "DONE", title = T.pack title, authors = mkAuthors authors }

getMetaArticle :: T.Text -> Either ParseError Article
getMetaArticle str = parse agaParser "(unknown)" str

parseArticle :: C.ByteString -> Either ParseError Article
parseArticle charmesh = getMetaArticle $ TE.decodeUtf8 $ B.concat $ BL.toChunks $ IConv.convert "CP1251" "UTF-8" charmesh