{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Handler.Movies where

import Data.Aeson (ToJSON, defaultOptions, fieldLabelModifier, genericToJSON, toJSON)
import Data.Char (toLower)
import Data.Time.Calendar (Day, fromGregorian)
import qualified Effect.Database as Database
import qualified Effect.Print as Print
import Servant.API (FromHttpApiData, Get, JSON, QueryParam, parseQueryParam, (:>))

type MovieAPI = "movies" :> QueryParam "sortBy" SortBy :> Get '[JSON] [Movie]

data SortBy = Rating | ReleasedDate
  deriving stock (Show, Eq)

instance FromHttpApiData SortBy where
  parseQueryParam text =
    case text of
      "rating" -> Right Rating
      "released_date" -> Right ReleasedDate
      _ -> Left $ "Failed: " <> text

data Movie = Movie
  { id :: Int32
  , title :: Text
  , releasedDate :: Day
  , rating :: Int32
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Movie where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = map toLower . drop (length ("movie" :: String))
        }

-- movies :: [Movie]
-- movies =
--   [ Movie {title = "Starwars", releasedDate = fromGregorian 2000 1 1, rating = 5}
--   , Movie {title = "Red Panda", releasedDate = fromGregorian 2010 12 22, rating = 1}
--   ]

moviesHandler :: (Database.MonadMovie m, Print.MonadPrint m) => Maybe SortBy -> m [Movie]
moviesHandler sortBy =
  Print.print sortBy
    >> Database.listMovies
    >>= (pure . fmap movieByTable)
  where
    movieByTable :: (Int32, Text, Day, Int32) -> Movie
    movieByTable (_id, title, releasedDate, rating) =
      Movie _id title releasedDate rating
