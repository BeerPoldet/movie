{-# LANGUAGE RecordWildCards #-}

module App.Server where

import qualified App.Config as Config
import qualified App.Env as Env
import qualified App.Monad as Monad
import qualified Core.Handler.Movies as Movies
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Server (
  Application,
  Context (EmptyContext),
  Server,
  hoistServer,
  -- serve,
  serveWithContextT,
 )

type API = Movies.MovieAPI

run :: Env.Env -> IO ()
run env@Env.Env {..} =
  Warp.run port app
  where
    port :: Int
    port = Config.serverConfigPort $ Config.configServer $ envConfig

    app :: Application
    app = serveWithContextT api EmptyContext id $ server

    api = Proxy @API

    server :: Server Movies.MovieAPI
    server = liftIO . Monad.runAppM env . Movies.moviesHandler
