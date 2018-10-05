module Data.Task.Equality where


import Preload

import Data.Task

import System.IO.Unsafe


infix 1 ===



-- Equivallence ----------------------------------------------------------------


(===) :: Basic a => MonadRef l m => TaskT l m a -> TaskT l m a -> m Bool
t1 === t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  pure $ v1 == v2



-- Properties ------------------------------------------------------------------

-- Monoidal functor --


prop_pair_left_identity :: TaskIO Int -> Bool
prop_pair_left_identity t = unsafePerformIO $
  tmap snd (edit () |&| t) === t


prop_pair_right_identity :: TaskIO Int -> Bool
prop_pair_right_identity t = unsafePerformIO $
  tmap fst (t |&| edit ()) === t


prop_pair_associativity :: TaskIO Int -> TaskIO Int -> TaskIO Int -> Bool
prop_pair_associativity r s t = unsafePerformIO $
  tmap assoc (r |&| (s |&| t)) === (r |&| s) |&| t



-- Alternative functor --


prop_choose_left_identity :: TaskIO Int -> Bool
prop_choose_left_identity t = unsafePerformIO $
  fail |!| t === t


prop_choose_right_identity :: TaskIO Int -> Bool
prop_choose_right_identity t = unsafePerformIO $
  t |!| fail === t


prop_choose_associativity :: TaskIO Int -> TaskIO Int -> TaskIO Int -> Bool
prop_choose_associativity r s t = unsafePerformIO $
  r |!| (s |!| t) === (r |!| s) |!| t


prop_choose_left_absorbtion :: TaskIO Int -> Bool
prop_choose_left_absorbtion t = unsafePerformIO $
  fail >>! (\x -> t) === fail


prop_choose_left_catch :: Int -> TaskIO Int -> Bool
prop_choose_left_catch x t = unsafePerformIO $
  edit x |!| t === edit x



-- Helpers --


assoc :: ( a, ( b, c ) ) -> ( ( a, b ), c )
assoc ( a, ( b, c ) ) = ( ( a, b ), c )
