{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta.Parsers (agaParser) where
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

authorParser :: Parser Author
authorParser = do
    name <- many1 (noneOf ",:\r\n\8212\8213")
    many (oneOf ":,\8212\8213")
    return Author{firstname = T.strip $ T.pack name,lastname = T.empty, username = T.empty}

authorsParser :: Parser [Author]
authorsParser = do
    string "Author: "
    authors <- many1 authorParser
    return $ map fixAuthor $ filter knownAuthorExceptions authors

titleParser :: Parser String
titleParser = do
    string "Title: "
    title <- many (noneOf "\r\n") 
    return title

agaParser :: Parser Article
agaParser = do
    manyTill anyChar (try (string "<agahidd"))
    eol
    authors <- optionMaybe authorsParser
    eol
    title <- titleParser
    manyTill anyChar (try (string "endhidd>"))
    return Article { text = T.empty, title = T.pack title, authors = liftAuthors authors }