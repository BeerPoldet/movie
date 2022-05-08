module Movie.App where

import qualified Movie.App.Config as Config
import qualified Movie.App.Database as Database
import qualified Movie.App.Env as Env
import qualified Movie.App.Server as Server
import qualified Movie.App.Monad as Monad

main :: IO ()
main = do
  cfg <- Config.loadConfig
  Monad.printM cfg
  let postgresConfig = Config.configPostgres $ cfg
  migrationResult <- Database.migrate postgresConfig
  Monad.printM migrationResult
  dbConnection <- Database.connect postgresConfig
  Server.run $ Env.Env cfg dbConnection
