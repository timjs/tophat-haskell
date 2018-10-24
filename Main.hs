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
  quickCheck (prop_choose_left_identity (-||-))
  quickCheck (prop_choose_right_identity (-||-))
  quickCheck (prop_choose_associativity (-||-))
  quickCheck (prop_choose_left_catch (-||-))
  quickCheck (prop_choose_idempotent (-||-))
  quickCheck $ expectFailure (prop_choose_commutative (-||-))
  quickCheck (prop_choose_distributive (-||-))

  putText "\n>>> Properties on external choice:"
  quickCheck $ expectFailure (prop_choose_left_identity (-??-))
  quickCheck $ expectFailure (prop_choose_right_identity (-??-))
  quickCheck (prop_choose_associativity (-??-))
  quickCheck $ expectFailure (prop_choose_left_catch (-??-))
  quickCheck $ expectFailure (prop_choose_idempotent (-??-))
  quickCheck (prop_choose_commutative (-??-))
  quickCheck (prop_choose_distributive (-??-))

  putText "\n>>> Properties on steps:"
  quickCheck (prop_step_left_identity (>>-))
  quickCheck (prop_step_right_identity (>>-))
  quickCheck (prop_step_assocaitivity (>>-))
  quickCheck (prop_step_left_anihilation (>>-))
  quickCheck (prop_step_left_absorption (>>-))

  putText "\n>>> Properties on external steps:"
  quickCheck $ expectFailure (prop_step_left_identity (>>?))
  quickCheck $ expectFailure (prop_step_right_identity (>>?))
  quickCheck (prop_step_assocaitivity (>>?))
  quickCheck (prop_step_left_anihilation (>>?))
  quickCheck (prop_step_left_absorption (>>?))
