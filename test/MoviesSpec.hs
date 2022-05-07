module MoviesSpec where

import Data.Time.Calendar (Day, fromGregorian)
import Movie.Movies (MonadMovie (..), Movie (..), SortBy (..), moviesHandler)
import Movie.Print (MonadPrint (..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

newtype TestEmptyMovie a = TestEmptyMovie
  { unTestEmptyMovie :: IO a
  }
  deriving newtype (Functor, Applicative, Monad)

instance MonadMovie TestEmptyMovie where
  listMovies = TestEmptyMovie $ pure $ []

instance MonadPrint TestEmptyMovie where
  print = pure . const ()

newtype TestSimpleMovie a = TestSimpleMovie
  { unTestSimpleMovie :: IO a
  }
  deriving newtype (Functor, Applicative, Monad)

instance MonadMovie TestSimpleMovie where
  listMovies =
    TestSimpleMovie $
      pure $
        [ (1, "Starwars", fromGregorian 2000 1 1, 5)
        ]

instance MonadPrint TestSimpleMovie where
  print = pure . const ()

spec :: Spec
spec =
  describe "moviesHandler" $ do
    it "returns empty movie list" $ do
      movies <- unTestEmptyMovie $ moviesHandler $ Just Rating
      movies `shouldBe` []

    it "returns movie list" $ do
      movies <- unTestSimpleMovie $ moviesHandler $ Just ReleasedDate
      movies
        `shouldBe` [ Movie
                       { Movie.Movies.id = 1,
                         title = "Starwars",
                         releasedDate = fromGregorian 2000 1 1,
                         rating = 5
                       }
                   ]
