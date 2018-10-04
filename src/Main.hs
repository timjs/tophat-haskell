module Main where


import Protolude

import Data.Task
import Data.Task.Internal ()

import System.IO.Unsafe

import Test.QuickCheck


main :: IO ()
main =
  verboseCheck prop_equal_values
  where
    prop_equal_values :: Task Int -> Bool
    prop_equal_values t = unsafePerformIO $ do
      x <- value t
      y <- value t
      pure $ x == y
