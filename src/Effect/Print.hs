module Effect.Print where

import App.Monad (AppM)
import Text.Pretty.Simple
import Prelude hiding (print)

class Monad m => MonadPrint m where
  print :: Show a => a -> m ()

instance MonadPrint (AppM IO) where
  print = pPrint

instance MonadPrint IO where
  print = printM

printM :: (MonadIO m, Show a) => a -> m ()
printM = pPrint

