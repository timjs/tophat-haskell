module Main where


import Protolude

import Data.Task
import Data.Task.Equality

import Test.QuickCheck


main :: IO ()
main = do
  putText "\n>>> Basic properties:"
  quickCheck prop_equal_val
  quickCheck prop_failing_preserved

  putText "\n>>> Properties on pairs:"
  quickCheck prop_pair_left_identity
  quickCheck prop_pair_right_identity
  quickCheck prop_pair_associativity
  quickCheck prop_pair_swap

  putText "\n>>> Properties on choice:"
  quickCheck prop_choose_left_identity
  quickCheck prop_choose_right_identity
  quickCheck prop_choose_associativity
  quickCheck prop_choose_left_absorbtion
  quickCheck prop_choose_left_catch
  quickCheck prop_choose_idempotent
  quickCheck $ expectFailure prop_choose_not_commutative

  putText "\n>>> Properties on external choice:"
  quickCheck prop_pick_distributive

  putText "\n>>> Properties on steps:"
  quickCheck (prop_step_left_identity (>>-))
  quickCheck (prop_step_right_identity (>>-))
  quickCheck (prop_step_assocaitivity (>>-))
  quickCheck (prop_step_left_anihilation (>>-))

  putText "\n>>> Properties on external steps:"
  quickCheck $ expectFailure (prop_step_left_identity (>>?))
  quickCheck $ expectFailure (prop_step_right_identity (>>?))
  quickCheck (prop_step_assocaitivity (>>?))
  quickCheck (prop_step_left_anihilation (>>?))
