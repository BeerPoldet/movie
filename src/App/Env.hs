module App.Env where

import qualified App.Config as Config
import Database.PostgreSQL.Typed (PGConnection)

data Env = Env
  { envConfig :: Config.Config
  , envDB :: PGConnection
  }
