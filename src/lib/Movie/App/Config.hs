{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Movie.App.Config where

import Conferer qualified

data Config = Config
  { configPostgres :: PostgresConfig
  , configServer :: ServerConfig
  }
  deriving stock (Show, Eq, Generic)

instance Conferer.FromConfig Config

data PostgresConfig = PostgresConfig
  { postgresConfigHost :: ByteString
  , postgresConfigPort :: Int
  , postgresConfigDatabase :: ByteString
  , postgresConfigUsername :: ByteString
  , postgresConfigPassword :: ByteString
  }
  deriving stock (Show, Eq, Generic)

instance Conferer.FromConfig PostgresConfig

data ServerConfig = ServerConfig
  { serverConfigPort :: Int
  }
  deriving stock (Show, Eq, Generic)

instance Conferer.FromConfig ServerConfig

loadConfig :: IO Config
loadConfig =
  Conferer.mkConfig "movi"
    >>= \conf ->
      Config
        <$> Conferer.unsafeFetchKey conf "postgres"
        <*> Conferer.unsafeFetchKey conf "server"
