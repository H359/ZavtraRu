{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta (parseArticle, ltrim, rtrim, strip, Article(..)) where
import Gazeta.Types
import Gazeta.Utils
import Gazeta.Parsers
import qualified Data.Text as T
import Text.Parsec.Error
import Text.Parsec.Prim
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Text.Pandoc as PD

getMetaArticle :: T.Text -> Article
getMetaArticle str = case parse agaParser "(agahidd)" str of
    Left  e -> Article{articleText=str, articleAuthors=[], articleUrl="", articleTitle="", articlePubdate=""}
    Right s -> s

special :: String -> Bool
special x = (head x == '[') || ((head . reverse) x == ']')

meta :: String -> Bool
meta x = x == "Title:" || x == "No:" || x == "Date: " || x == "Author: "

hasToOmit :: String -> Bool
hasToOmit x = (special x) || (meta x)

cleanText :: [PD.Inline] -> [PD.Inline]
cleanText (PD.Link _ _ : xs) = xs
cleanText (PD.Image _ _ : xs) = xs
cleanText (PD.Str "__" : xs) = xs
cleanText (PD.Str x : xs) = if hasToOmit x then xs else (PD.Str x) : xs
cleanText (PD.LineBreak : PD.LineBreak : xs) = PD.LineBreak : xs
cleanText (PD.Space : PD.LineBreak : xs) = PD.LineBreak : xs
cleanText (PD.LineBreak : PD.Space : xs) = PD.LineBreak : xs
cleanText xs = xs

processText :: T.Text -> T.Text
processText  = T.pack . writer . cleaner . reader . T.unpack
    where reader = PD.readHtml PD.defaultParserState
	  cleaner x = PD.bottomUp cleanText x
	  writer = PD.writeHtmlString PD.defaultWriterOptions
	  --writer = PD.writeNative PD.defaultWriterOptions

addText :: Article -> Article
addText a = Article{ articlePubdate=articlePubdate a, articleTitle=articleTitle a, articleUrl=articleUrl a, articleAuthors=articleAuthors a, articleText=(processText . articleText) a}

parseArticle :: C.ByteString -> Article
parseArticle charmesh = addText metaArticle
    where metaArticle = getMetaArticle charmesh8
	  charmesh8   = getCharMesh8 charmesh