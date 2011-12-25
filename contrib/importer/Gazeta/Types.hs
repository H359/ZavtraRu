{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta.Types (Article(..)) where
import qualified Data.Text as T
import qualified Data.Map as Map

data Article = Article {
    articleUrl     :: String,
    articlePubdate :: String,
    articleTitle   :: String,
    articleAuthors :: [String],
    articleText    :: T.Text
}