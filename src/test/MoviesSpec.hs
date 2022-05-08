module MoviesSpec where

import Data.Time.Calendar (Day, fromGregorian)
import Movie.Movies (MonadListMovie (..), Movie (..), SortBy (..), moviesHandler)
import Movie.Print (MonadPrint (..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

spec :: Spec
spec =
  describe "moviesHandler" $ do
    it "returns empty movie list" $ do
      let movieData = []
      movies <- runTestMovie movieData $ moviesHandler $ Just Rating
      movies `shouldBe` []

    it "returns movie list" $ do
      let movieData =
            [ (1, "Starwars", fromGregorian 2000 1 1, 5)
            ]
      movies <- runTestMovie movieData $ moviesHandler $ Just ReleasedDate
      movies
        `shouldBe` [ Movie
                      { Movie.Movies.id = 1
                      , title = "Starwars"
                      , releasedDate = fromGregorian 2000 1 1
                      , rating = 5
                      }
                   ]

newtype TestMovie a = TestMovie
  { unTestMovie :: ReaderT [(Int32, Text, Day, Int32)] IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader [(Int32, Text, Day, Int32)]
    )

instance MonadListMovie TestMovie where
  listMovies = ask

instance MonadPrint TestMovie where
  print = pure . const ()

runTestMovie :: [(Int32, Text, Day, Int32)] -> TestMovie a -> IO a
runTestMovie movies monad = runReaderT (unTestMovie monad) movies

