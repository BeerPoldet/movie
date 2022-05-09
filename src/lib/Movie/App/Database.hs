{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Movie.App.Database where

import Data.Text as T
import Data.Text.Encoding as T
import Database.PostgreSQL.Simple as PSQLS (
  connect,
  connectDatabase,
  connectHost,
  connectPassword,
  connectPort,
  connectUser,
  defaultConnectInfo,
 )
import Database.PostgreSQL.Simple.Migration as Migration (
  MigrationCommand (MigrationDirectory, MigrationInitialization),
  MigrationResult,
  defaultOptions,
  runMigrations,
 )
import Database.PostgreSQL.Typed
import Movie.App.Config (PostgresConfig (..))

connect :: PostgresConfig -> IO PGConnection
connect PostgresConfig {..} =
  pgConnect
    defaultPGDatabase
      { pgDBUser = postgresConfigUsername
      , pgDBPass = postgresConfigPassword
      , pgDBName = postgresConfigDatabase
      , pgDBAddr = Left (fromByteString postgresConfigHost, show postgresConfigPort)
      }

disconnect :: PGConnection -> IO ()
disconnect = pgDisconnect

migrate :: PostgresConfig -> IO (Migration.MigrationResult String)
migrate cfg = do
  connection <-
    PSQLS.connect $
      PSQLS.defaultConnectInfo
        { PSQLS.connectHost = fromByteString $ postgresConfigHost cfg
        , PSQLS.connectPort = fromIntegral $ postgresConfigPort cfg
        , PSQLS.connectUser = fromByteString $ postgresConfigUsername cfg
        , PSQLS.connectPassword = fromByteString $ postgresConfigPassword cfg
        , PSQLS.connectDatabase = fromByteString $ postgresConfigDatabase cfg
        }
  Migration.runMigrations connection Migration.defaultOptions $
    [ Migration.MigrationInitialization
    , Migration.MigrationDirectory "migrations"
    ]

fromByteString :: ByteString -> String
fromByteString = T.unpack . T.decodeUtf8
