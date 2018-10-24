module Main where


import Protolude

import Data.Task
import Data.Task.Equality

import System.IO.Unsafe

import Test.QuickCheck


main :: IO ()
main = do
  putText ">>> Valid properties:"

  quickCheck prop_equal_val
  quickCheck prop_normalising_preserves_failing

  quickCheck prop_pair_left_identity
  quickCheck prop_pair_right_identity
  quickCheck prop_pair_associativity

  quickCheck prop_choose_left_identity
  quickCheck prop_choose_right_identity
  quickCheck prop_choose_associativity
  quickCheck prop_choose_left_absorbtion
  quickCheck prop_choose_left_catch

  quickCheck prop_step_left_identity
  quickCheck prop_step_right_identity
  quickCheck prop_step_assocaitivity

  putText ""
  putText ">>> Invalid properties:"

  quickCheck prop_choose_not_commutative

  where

    prop_equal_val :: TaskIO Int -> Bool
    prop_equal_val t = unsafePerformIO $ do
      x <- value t
      y <- value t
      pure $ x == y

    prop_normalising_preserves_failing :: TaskIO Int -> Bool
    prop_normalising_preserves_failing t = unsafePerformIO $ do
      t' <- normalise t
      pure $ failing t == failing t'

    -- prop_norm_val :: Task Int -> Bool
    -- prop_norm_val t = unsafePerformIO $ do
    --   v <- value t
    --   t' <- normalise t
    --   v' <- value t'
    --   pure $ v == v'
