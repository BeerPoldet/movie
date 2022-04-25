{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Database where

import App.Config (PostgresConfig (..))
import Database.PostgreSQL.Typed

connect :: PostgresConfig -> IO PGConnection
connect PostgresConfig {..} =
  pgConnect
    defaultPGDatabase
      { pgDBUser = postgresConfigUsername
      , pgDBPass = postgresConfigPassword
      }

disconnect :: PGConnection -> IO ()
disconnect = pgDisconnect
