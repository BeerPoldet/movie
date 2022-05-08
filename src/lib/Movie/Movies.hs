{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Movie.Movies where

import Data.Aeson (
  FromJSON,
  ToJSON,
  defaultOptions,
  fieldLabelModifier,
  genericParseJSON,
  genericToJSON,
  parseJSON,
  toJSON,
 )
import Data.Char (toLower)
import Data.Time.Calendar (Day, fromGregorian)
import Movie.Print qualified as Print
import Servant.API (
  FromHttpApiData,
  Get,
  JSON,
  Post,
  QueryParam,
  ReqBody,
  parseQueryParam,
  (:<|>),
  (:>),
 )

type MovieAPI =
  "movies"
    :> ( QueryParam "sortBy" SortBy :> Get '[JSON] [Movie]
          :<|> ReqBody '[JSON] CreateMovie :> Post '[JSON] ()
       )

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

-- { fieldLabelModifier = map toLower . drop (length ("movie" :: String))
-- }

-- movies :: [Movie]
-- movies =
--   [ Movie {title = "Starwars", releasedDate = fromGregorian 2000 1 1, rating = 5}
--   , Movie {title = "Red Panda", releasedDate = fromGregorian 2010 12 22, rating = 1}
--   ]

class Monad m => MonadListMovie m where
  listMovies :: m [(Int32, Text, Day, Int32)]

listMoviesHandler :: (MonadListMovie m, Print.MonadPrint m) => Maybe SortBy -> m [Movie]
listMoviesHandler sortBy =
  Print.print sortBy
    >> listMovies
    <&> fmap movieByTable
  where
    movieByTable :: (Int32, Text, Day, Int32) -> Movie
    movieByTable (_id, title, releasedDate, rating) =
      Movie _id title releasedDate rating

data CreateMovie = CreateMovie
  { createMovieTitle :: Text
  , createMovieReleasedDate :: Day
  , createMovieRating :: Int32
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CreateMovie where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = map toLower . drop (length ("createMovie" :: String))
        }

instance FromJSON CreateMovie where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = map toLower . drop (length ("createMovie" :: String))
        }

class Monad m => MonadCreateMovie m where
  createMovie :: CreateMovie -> m ()

createMovieHandler :: (MonadCreateMovie m, Print.MonadPrint m) => CreateMovie -> m ()
createMovieHandler m =
  Print.print m
    >> createMovie m
