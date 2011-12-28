{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta.Parsers (agaParser, ltrim, rtrim, strip) where
import Gazeta.Types
import Gazeta.Utils
import qualified Data.Text as T
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Error

instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons

type Parser = Parsec T.Text ()
type GenParser t st = Parsec T.Text st

eol = many (oneOf "\r\n")

strip :: String -> String
strip = ltrim . rtrim

ltrim :: String -> String
ltrim = dropWhile (\s -> s == ' ')

rtrim :: String -> String
rtrim = reverse . ltrim . reverse

authorParser :: Parser String
authorParser = do
    name <- many1 (noneOf ",:\r\n\8212\8213")
    many (oneOf ":,\8212\8213")
    return $ strip name

authorsParser :: Parser [String]
authorsParser = do
    string "Author: "
    authors <- many1 authorParser
    return $ map fixAuthor $ filter knownAuthorExceptions authors

titleParser :: Parser String
titleParser = do
    string "Title: "
    title <- many (noneOf "\r\n") 
    return title

dateParser :: Parser String
dateParser = do
    string "Date: "
    day <- many1 (noneOf "-")
    string "-"
    month <- many1 (noneOf "-")
    string "-"
    year <- many1 (noneOf "\r\n")
    return $ makethDate day month year

noParser :: Parser String
noParser = do
    string "No: "
    no <- many (noneOf "\r\n")
    return no

agaParser :: Parser Article
agaParser = do
    s1 <- manyTill anyChar (try (string "<agahidd"))
    eol
    authors <- optionMaybe authorsParser
    eol
    title <- titleParser
    eol
    no <- noParser
    eol
    date <- dateParser
    manyTill anyChar (try (string "endhidd>"))
    s2 <- many1 anyChar
    return Article { articleUrl = "", articleText = T.pack (s1 ++ s2), articleTitle = title, articleAuthors = liftAuthors authors, articlePubdate=date }