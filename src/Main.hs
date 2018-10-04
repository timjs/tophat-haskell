module Main where


import Protolude

import Data.Task
import Data.Task.Internal ()

import System.IO.Unsafe

import Test.QuickCheck


main :: IO ()
main = do
  verboseCheck prop_equal_val
  verboseCheck prop_normalising_preserves_failing
  where

    prop_equal_val :: Task Int -> Bool
    prop_equal_val t = unsafePerformIO $ do
      x <- value t
      y <- value t
      pure $ x == y

    prop_normalising_preserves_failing :: Task Int -> Bool
    prop_normalising_preserves_failing t = unsafePerformIO $ do
      t' <- normalise t
      pure $ failing t == failing t'
      
    prop_norm_val :: Task Int -> Bool
    prop_norm_val t = unsafePerformIO $ do
      v <- value t
      t' <- normalise t
      v' <- value t'
      pure $ v == v'
