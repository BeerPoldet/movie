module App.Monad where

import qualified App.Config as Config
import App.Env (Env (..))
import qualified App.Env as Env
import Database.PostgreSQL.Typed (PGConnection)

newtype AppM m a = AppM
  { unAppM :: ReaderT Env m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

runAppM :: Env -> AppM IO a -> IO a
runAppM env appM = runReaderT (unAppM appM) env

config :: (MonadReader Env.Env m) => m Config.Config
config = ask >>= return . envConfig

dbConnection :: (MonadReader Env.Env m) => m PGConnection
dbConnection = ask >>= return . envDB
