module Main where


import Protolude

import Data.Task.Equality

import Test.QuickCheck


main :: IO ()
main = do
  putText ">>> Valid properties:"

  quickCheck prop_equal_val
  quickCheck prop_failing_preserved

  quickCheck prop_pair_left_identity
  quickCheck prop_pair_right_identity
  quickCheck prop_pair_associativity
  quickCheck prop_pair_swap

  quickCheck prop_choose_left_identity
  quickCheck prop_choose_right_identity
  quickCheck prop_choose_associativity
  quickCheck prop_choose_left_absorbtion
  quickCheck prop_choose_left_catch
  quickCheck prop_choose_idempotent

  quickCheck prop_step_left_identity
  quickCheck prop_step_right_identity
  quickCheck prop_step_assocaitivity
  quickCheck prop_step_left_anihilation

  quickCheck prop_pick_distributive
  
  putText ""
  putText ">>> Invalid properties:"

  quickCheck prop_choose_not_commutative
