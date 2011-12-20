{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta.Types (Issue(..), Article(..), Author(..), Rubric(..)) where
import qualified Data.Text as T

data Issue = Issue {
    pub_date :: Integer,
    articles :: [Article],
    rubrics  :: [Rubric]
}

data Rubric = Rubric {
    rtitle :: T.Text,
    urls :: [T.Text]
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