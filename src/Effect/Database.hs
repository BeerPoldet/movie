{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.Database where

import qualified App.Monad as Monad
import Data.Time (Day)
import Data.Time.Calendar (fromGregorian)
import Database.PostgreSQL.Typed

class HasPerson m c | m -> c where
  listPerson :: c -> Text -> m [(Int32, Text)]

-- instance HasPerson IO PGConnection where
--   listPerson c name =
--     return [(1, "hello")]

-- pgQuery c [pgSQL|SELECT id, name FROM people WHERE name=${name}|]

-- class Monad m => HasMovie m c | m -> c where
--   listMovies :: c -> m [(Int32, Text, Day, Int32)]

-- instance HasMovie IO PGConnection where
--   listMovies c =
--     return [(1, "hello", fromGregorian 2010 12 22, 3)]
-- pgQuery c [pgSQL|SELECT id, title, released_date, rating FROM movies|]

class Monad m => MonadMovie m where
  listMovies :: m [(Int32, Text, Day, Int32)]

-- instance MonadMovie (ReaderT PGConnection IO) where
--   listMovies = do
--     c <- ask
--     lift $ return [(1, "hello", fromGregorian 2010 12 22, 3)]
--     lift $ pgQuery c [pgSQL|SELECT id, title, released_date, rating FROM movies|]

instance MonadMovie (Monad.AppM IO) where
  listMovies = do
    c <- Monad.dbConnection
    -- lift $ return [(1, "hello", fromGregorian 2010 12 22, 3)]
    lift $ pgQuery c [pgSQL|SELECT id, title, released_date, rating FROM movies|]

-- listMovies'' = Monad.AppM $ withReaderT Env.envDB listMovies''

-- listMovies' :: WithDB m => m [(Int32, Text, Day, Int32)]
-- listMovies' = do
--   c <- Monad.dbConnection
--   -- return [(1, "hello", fromGregorian 2010 12 22, 3)]
--   liftIO $ pgQuery c [pgSQL|SELECT id, title, released_date, rating FROM movies|]

-- instance (MonadReader PGConnection m, MonadIO m) => MonadMovie m PGConnection where
--   listMovies' = do
--     c <- ask
--     liftIO $ pgQuery c [pgSQL|SELECT id, title, released_date, rating FROM movies|]
