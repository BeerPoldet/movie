{-# LANGUAGE RecordWildCards #-}

module Movie.App.Server where

import qualified Movie.App.Config as Config
import qualified Movie.App.Env as Env
import qualified Movie.App.Monad as Monad
import qualified Movie.Movies as Movies
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
