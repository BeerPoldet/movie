{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Movie.App.Monad where

import Control.Monad.Error.Class (MonadError)
import Database.PostgreSQL.Typed (PGConnection, pgQuery, pgSQL)
import Movie.App.Config qualified as Config
import Movie.App.Env (Env (..))
import Movie.App.Env qualified as Env
import Movie.Movies
import Movie.Print
import Servant.Server (ServerError)
import Text.Pretty.Simple (pPrint)
import Prelude hiding (print)

newtype AppM m a = AppM
  { unAppM :: ReaderT Env (ExceptT AppError m) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError AppError)

instance MonadTrans AppM where
  lift = AppM . lift . lift

newtype AppError = ServerError ServerError

runAppM :: Env -> AppM IO a -> IO (Either AppError a)
runAppM env appM = runExceptT $ runReaderT (unAppM appM) env

config :: (MonadReader Env.Env m) => m Config.Config
-- config = ask >>= return . envConfig
config = fmap envConfig ask

dbConnection :: (MonadReader Env.Env m) => m PGConnection
dbConnection = fmap envDB ask

--
-- Effects
--

--
-- MonadListMovie
--

instance MonadListMovie (AppM IO) where
  listMovies = do
    c <- dbConnection
    -- lift $ return [(1, "hello", fromGregorian 2010 12 22, 3)]
    lift $
      pgQuery
        c
        [pgSQL|
          SELECT 
            id,
            title,
            released_date,
            rating 
          FROM movies
        |]

instance MonadCreateMovie (AppM IO) where
  createMovie CreateMovie {..} = do
    c <- dbConnection
    _ <-
      lift $
        pgQuery
          c
          [pgSQL|
            INSERT INTO movies (title, released_date, rating) 
            VALUES (
              ${createMovieTitle},
              ${createMovieReleasedDate},
              ${createMovieRating}
            )
        |]
    return ()

instance MonadUpdateMovie (AppM IO) where
  updateMovie UpdateMovie {..} = do
    c <- dbConnection
    _ <-
      lift $
        pgQuery
          c
          [pgSQL|
            UPDATE movies
            SET title = COALESCE(${updateMovieTitle}, title),
                released_date = COALESCE(${updateMovieReleasedDate}, released_date),
                rating = COALESCE(${updateMovieRating}, rating)
            WHERE id=${updateMovieId}
          |]
    return ()

--
-- MonadPrint
--

instance MonadPrint (AppM IO) where
  print = pPrint

printM :: (MonadIO m, Show a) => a -> m ()
printM = pPrint
