module Movie.App.Env where

import qualified Movie.App.Config as Config
import Database.PostgreSQL.Typed (PGConnection)

data Env = Env
  { envConfig :: Config.Config
  , envDB :: PGConnection
  }
