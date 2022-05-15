{-# LANGUAGE RecordWildCards #-}

module Movie.App.Server where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (withExceptT)
import Movie.App.Config qualified as Config
import Movie.App.Env qualified as Env
import Movie.App.Monad qualified as Monad
import Movie.Movies qualified as Movies
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Get, Header, Headers (..), JSON, NoContent (NoContent), ReqBody, StdMethod (..), Verb, (:<|>) (..), (:>))
import Servant.Server (
  Application,
  Context (..),
  Handler (Handler),
  ServerError,
  ServerT,
  err401,
  hoistServerWithContext,
  serveWithContext,
 )

import Data.Aeson (FromJSON, ToJSON)
import Servant.Auth.Server

type API auths = (Servant.Auth.Server.Auth auths User :> Protected) :<|> Unprotected :<|> Movies.MovieAPI

run :: Env.Env -> IO ()
run env@Env.Env {..} = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      port :: Int
      port = Config.serverConfigPort $ Config.configServer envConfig

      app :: Application
      app =
        serveWithContext api cfg $
          hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) appMToHandler $
            server defaultCookieSettings jwtCfg

      api = Proxy @(API '[JWT])

      server :: CookieSettings -> JWTSettings -> ServerT (API '[JWT]) (Monad.AppM IO)
      server cs jwt =
        protected
          :<|> unprotected cs jwt
          :<|> Movies.listMoviesHandler
          :<|> Movies.createMovieHandler
          :<|> Movies.updateMovieHandler

      appMToHandler :: Monad.AppM IO a -> Handler a
      appMToHandler appM = Handler $ withExceptT handleError $ ExceptT $ Monad.runAppM env appM

      handleError :: Monad.AppError -> ServerError
      handleError (Monad.ServerError e) = e

  Warp.run port app

data User = User {name :: String, email :: String}
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login {username :: String, password :: String}
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

type Protected =
  "name" :> Get '[JSON] String
    :<|> "email" :> Get '[JSON] String

protected :: Servant.Auth.Server.AuthResult User -> ServerT Protected (Monad.AppM IO)
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected (Servant.Auth.Server.Authenticated user) = return (name user) :<|> return (email user)
-- Otherwise, we return a 401.
protected _ =
  throwError (Monad.ServerError err401)
    :<|> throwError (Monad.ServerError err401)

type Unprotected =
  "login"
    :> ReqBody '[JSON] Login
    :> Verb
        'POST
        204
        '[JSON]
        ( Headers
            '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
            NoContent
        )

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected (Monad.AppM IO)
unprotected = checkCreds

checkCreds ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  Monad.AppM
    IO
    ( Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
        NoContent
    )
checkCreds cookieSettings jwtSettings (Login "Ali Baba" "Open Sesame") = do
  -- Usually you would ask a database for the user info. This is just a
  -- regular servant handler, so you can follow your normal database access
  -- patterns (including using 'enter').
  let usr = User "Ali Baba" "ali@email.com"
  mApplyCookies <- lift $ liftIO $ acceptLogin cookieSettings jwtSettings usr
  case mApplyCookies of
    Nothing -> throwError $ Monad.ServerError err401
    Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError $ Monad.ServerError err401
