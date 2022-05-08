{-# LANGUAGE RecordWildCards #-}

module Movie.App.Server where

import Movie.App.Config qualified as Config
import Movie.App.Env qualified as Env
import Movie.App.Monad qualified as Monad
import Movie.Movies qualified as Movies
import Network.Wai.Handler.Warp qualified as Warp
import Servant ((:<|>) (..))
import Servant.Server (
  Application,
  Context (EmptyContext),
  Server,
  hoistServer,
  -- serve,
  serveWithContextT,
  Handler
 )

type API = Movies.MovieAPI

run :: Env.Env -> IO ()
run env@Env.Env {..} =
  Warp.run port app
  where
    port :: Int
    port = Config.serverConfigPort $ Config.configServer envConfig

    app :: Application
    app = serveWithContextT api EmptyContext id server

    api = Proxy @API

    server :: Server Movies.MovieAPI
    server = appMToHandler . Movies.listMoviesHandler 
            :<|> appMToHandler . Movies.createMovieHandler

    appMToHandler :: Monad.AppM IO a -> Handler a
    appMToHandler appM = liftIO $ Monad.runAppM env appM
