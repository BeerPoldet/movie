module App where

import qualified App.Config as Config
import qualified App.Database as Database
import qualified App.Env as Env
import qualified App.Server as Server
import qualified Effect.Print as Print

main :: IO ()
main = do
  cfg <- Config.loadConfig
  Print.print cfg
  dbConnection <- Database.connect $ Config.configPostgres cfg
  Server.run $ Env.Env cfg dbConnection
