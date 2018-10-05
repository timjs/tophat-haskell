module Data.Task.Equality where


import Preload

import Data.Task

import System.IO.Unsafe


infix 4 ===



-- Equivallence ----------------------------------------------------------------


(===) :: Basic a => MonadRef l m => TaskT l m a -> TaskT l m a -> m Bool
t1 === t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  pure $ v1 == v2



-- Properties ------------------------------------------------------------------


prop_pair_left_identity :: Task Int -> Bool
prop_pair_left_identity t = unsafePerformIO $
  tmap snd (edit () |&| t) === t


prop_pair_right_identity :: Task Int -> Bool
prop_pair_right_identity t = unsafePerformIO $
  tmap fst (t |&| edit ()) === t


prop_pair_associativity :: Task Int -> Task Int -> Task Int -> Bool
prop_pair_associativity r s t = unsafePerformIO $
  tmap assoc (r |&| (s |&| t)) === (r |&| s) |&| t



-- Helpers --


assoc :: ( a, ( b, c ) ) -> ( ( a, b ), c )
assoc ( a, ( b, c ) ) = ( ( a, b ), c )
