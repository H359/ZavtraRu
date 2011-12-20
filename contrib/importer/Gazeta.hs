{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta (parseIssue, parseArticle, Issue(..), Article(..), Author(..), Rubric(..)) where
import Gazeta.Types
import Gazeta.Utils
import Gazeta.Parsers
import qualified Data.Text as T
import Text.Parsec.Error
import Text.Parsec.Prim
--import qualified Data.List as L
--import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as C
--import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as B
--import qualified Codec.Text.IConv as IConv
import qualified Text.Pandoc as PD

extractURL :: PD.Inline -> [T.Text]
extractURL (PD.Link h (u, _)) = [T.pack ((concatMap plain h) ++ " --> " ++ u)]
    where plain (PD.Str x) = x
	  plain (PD.Emph x) = concatMap plain x
	  plain (PD.Strong x) = concatMap plain x
	  plain (PD.Strikeout x) = concatMap plain x
	  plain (PD.Superscript x) = concatMap plain x
	  plain (PD.Subscript x) = concatMap plain x
	  plain (PD.SmallCaps x) = concatMap plain x
	  plain (PD.Quoted _ x) = concatMap plain x
	  plain (PD.Cite _ x) = concatMap plain x
	  plain (PD.Code _ x) = x
	  plain (PD.Space) = " "
	  plain (PD.EmDash) = " &mdash; "
	  plain (PD.EnDash) = " &ndash; "
	  plain (PD.Apostrophe) = "'"
	  plain (PD.Ellipses) = ""
	  plain (PD.LineBreak) = ""
	  plain (PD.Math _ x) = x
	  plain (PD.RawInline _ x) = x
	  --plain (PD.Link _ x) = x

extractURL _ = []

extractURLs :: PD.Pandoc -> [T.Text]
extractURLs = PD.queryWith extractURL

extractIssueInfo :: PD.Pandoc -> Issue
extractIssueInfo pandoc = Issue{pub_date=0, articles=[], rubrics=rubrics'}
    where rubrics' = [Rubric{rtitle=T.pack "tydy", urls=extractURLs pandoc}]

getMetaArticle :: T.Text -> Either ParseError Article
getMetaArticle str = parse agaParser "(agahidd)" str

parseIssueIndex :: C.ByteString -> Issue
parseIssueIndex charmesh = extractIssueInfo pandoc
    where pandoc = PD.readHtml PD.defaultParserState $ T.unpack $ getCharMesh8 charmesh

parseIssue :: C.ByteString -> [Article] -> Issue
parseIssue charmesh newarticles = addArticles (parseIssueIndex charmesh) newarticles

parseArticle :: C.ByteString -> Either ParseError Article
parseArticle charmesh = getMetaArticle $ getCharMesh8 charmesh