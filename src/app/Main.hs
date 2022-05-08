module Main where

import qualified Movie.App as App

{- |
 Main entry point.

 The `bin/run` script will invoke this function. See `.ghcid` file to change
 that.
-}
main :: IO ()
main = App.main
