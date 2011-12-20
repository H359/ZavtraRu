{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Gazeta where
-- encoding hackage failed to install :-/
--import Data.Encoding
--import Data.Encoding.CP1251
--import Data.Encoding.UTF8
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
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

commonFixer :: Char -> Char
commonFixer '“' = '\"'
commonFixer c = c

knownFullExceptions :: [T.Text]
knownFullExceptions = map T.pack exceptions
    where exceptions = ["ii съезд нпср", "студент", "профессор", "журналист", "д.и.н.", "д.м.н", "д.ф.н.", "д.э.н.", "дед из сибири"] ++
		       ["депутат госдумы", "депутат госдумы рф", "депутат госдумы россии", "депутат госдумы от фракции кпрф"] ++
		       ["депутат государственной думы", "депутат государственной думы рф", "профессор мгу", "доктор исторических наук"] ++
		       ["доктор медицинских наук", "доктор права", "доктор технических наук", "доктор философских наук", "доктор экономических наук"] ++
		       ["доцент", "эурналист", "инженер-физик", "кандидит биологических наук", "кандидат наук", "кандидат философских наук"] ++
		       ["капитан 1 ранга", "капитан i ранга", "адвокат", "актер", "юрист", "режиссер", "редакция", "сербский поэт", "старший эксперт"]

knownInitialExceptions :: [T.Text]
knownInitialExceptions = map T.pack exceptions
    where exceptions = ["специальный корреспондент", "сопредседатель координационного совета", "собственный корреспондент", "собственные корреспонденты"] ++
		       ["собкор", "председатель думской", "председатель коми", "председатель партии", "духовник"]

knownAuthorExceptions :: Author -> Bool
knownAuthorExceptions author = (authorName `L.notElem` knownFullExceptions) && (foldl (&&) True (map (\s -> not (s `T.isPrefixOf` authorName)) knownInitialExceptions))
    where authorName = T.toLower $ firstname author

fixAuthorName :: Author -> Author
fixAuthorName a = Author{firstname=firstname', lastname=lastname', username=username'}
    where fixName s  = T.map commonFixer s
	  firstname' = fixName (firstname a)
	  lastname'  = fixName (lastname a)
	  username'  = fixName (username a)

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
    return $ map fixAuthorName $ filter knownAuthorExceptions authors

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
    return Article { text = T.empty, title = T.pack title, authors = mkAuthors authors }

getMetaArticle :: T.Text -> Either ParseError Article
getMetaArticle str = parse agaParser "(unknown)" str

parseArticle :: C.ByteString -> Either ParseError Article
parseArticle charmesh = getMetaArticle $ TE.decodeUtf8 $ B.concat $ BL.toChunks $ IConv.convert "CP1251" "UTF-8" charmesh