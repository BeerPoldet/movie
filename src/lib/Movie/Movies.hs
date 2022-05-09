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
import Data.Time.Calendar (Day)
import Movie.Print qualified as Print
import Servant.API (
  FromHttpApiData,
  Get,
  JSON,
  Post,
  Put,
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
          :<|> ReqBody '[JSON] UpdateMovie :> Put '[JSON] ()
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
  { movieId :: Int32
  , movieTitle :: Text
  , movieReleasedDate :: Day
  , movieRating :: Int32
  }
  deriving stock (Show, Eq, Generic)

jsonFieldByMovieRecord :: String -> String
jsonFieldByMovieRecord s =
  case s of
    "movieId" -> "id"
    "movieTitle" -> "title"
    "movieReleasedDate" -> "released_date"
    "movieRating" -> "rating"
    _ -> s

instance ToJSON Movie where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldByMovieRecord
        }

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
    movieByTable (movieId, movieTitle, movieReleasedDate, movieRating) =
      Movie movieId movieTitle movieReleasedDate movieRating

data CreateMovie = CreateMovie
  { createMovieTitle :: Text
  , createMovieReleasedDate :: Day
  , createMovieRating :: Int32
  }
  deriving stock (Show, Eq, Generic)

jsonFieldByCreateMovieRecord :: String -> String
jsonFieldByCreateMovieRecord s =
  case s of
    "createMovieId" -> "id"
    "createMovieTitle" -> "title"
    "createMovieReleasedDate" -> "released_date"
    "createMovieRating" -> "rating"
    _ -> s

instance ToJSON CreateMovie where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldByCreateMovieRecord
        }

instance FromJSON CreateMovie where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldByCreateMovieRecord
        }

class Monad m => MonadCreateMovie m where
  createMovie :: CreateMovie -> m ()

createMovieHandler :: (MonadCreateMovie m, Print.MonadPrint m) => CreateMovie -> m ()
createMovieHandler m =
  Print.print m
    >> createMovie m

data UpdateMovie = UpdateMovie
  { updateMovieId :: Int32
  , updateMovieTitle :: Maybe Text
  , updateMovieReleasedDate :: Maybe Day
  , updateMovieRating :: Maybe Int32
  }
  deriving stock (Show, Eq, Generic)

jsonFieldByUpdateMovieRecord :: String -> String
jsonFieldByUpdateMovieRecord s =
  case s of
    "updateMovieId" -> "id"
    "updateMovieTitle" -> "title"
    "updateMovieReleasedDate" -> "released_date"
    "updateMovieRating" -> "rating"
    _ -> s

instance ToJSON UpdateMovie where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldByUpdateMovieRecord
        }

instance FromJSON UpdateMovie where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldByUpdateMovieRecord
        }

class Monad m => MonadUpdateMovie m where
  updateMovie :: UpdateMovie -> m ()

updateMovieHandler :: (MonadUpdateMovie m, Print.MonadPrint m) => UpdateMovie -> m ()
updateMovieHandler m = do
  Print.print m
  updateMovie m
