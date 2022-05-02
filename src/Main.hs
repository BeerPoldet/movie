module Main where

import Movie.App qualified as App
import Main.Utf8 qualified as Utf8

{- |
 Main entry point.

 The `bin/run` script will invoke this function. See `.ghcid` file to change
 that.
-}
main :: IO ()
main = App.main
