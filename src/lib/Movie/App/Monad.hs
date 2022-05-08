{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Movie.App.Monad where

import Data.Time.Calendar (fromGregorian)
import Database.PostgreSQL.Typed (PGConnection, pgQuery, pgSQL)
import Movie.App.Config qualified as Config
import Movie.App.Env (Env (..))
import Movie.App.Env qualified as Env
import Movie.Movies
import Movie.Print
import Text.Pretty.Simple (pPrint)
import Prelude hiding (print)

newtype AppM m a = AppM
  { unAppM :: ReaderT Env m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

runAppM :: Env -> AppM IO a -> IO a
runAppM env appM = runReaderT (unAppM appM) env

config :: (MonadReader Env.Env m) => m Config.Config
-- config = ask >>= return . envConfig
config = fmap ask envConfig

dbConnection :: (MonadReader Env.Env m) => m PGConnection
dbConnection = ask >>= return . envDB

--
-- Effects
--

--
-- MonadMovie
--

instance MonadMovie (AppM IO) where
  listMovies = do
    c <- dbConnection
    -- lift $ return [(1, "hello", fromGregorian 2010 12 22, 3)]
    lift $ pgQuery c [pgSQL|SELECT id, title, released_date, rating FROM movies|]

instance MonadCreateMovie (AppM IO) where
  createMovie movie@CreateMovie {..} = do
    c <- dbConnection
    lift $
      pgQuery
        c
        [pgSQL|
          INSERT INTO movies (title, released_date, rating) 
          VALUES (${createMovieTitle}, ${createMovieReleasedDate}, ${createMovieRating})
        |]
    return ()

--
-- MonadPrint
--

instance MonadPrint (AppM IO) where
  print = pPrint

printM :: (MonadIO m, Show a) => a -> m ()
printM = pPrint
