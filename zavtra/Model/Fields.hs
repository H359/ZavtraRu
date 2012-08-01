module Model.Fields where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Quasi

newtype Slug = Slug { unSlug :: Text}
  deriving (Show, Read, Eq, PathPiece, PersistField)

data Gender = Male | Female
  deriving (Show, Read, Eq)
derivePersistField "Gender"

data SupportedTextFormat = PlainHTML | Markdown
  deriving (Show, Read, Eq)
derivePersistField "SupportedTextFormat"
