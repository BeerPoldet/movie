{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module App.Config where

import qualified Conferer

data Config = Config
  { configPostgres :: PostgresConfig
  , configServer :: ServerConfig
  }
  deriving stock (Show, Eq, Generic)

instance Conferer.FromConfig Config

data PostgresConfig = PostgresConfig
  { postgresConfigUsername :: ByteString
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
