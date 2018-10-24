module Data.Task.Equality
  ( (===), (~=)
  , prop_equal_val, prop_failing_preserved
  , prop_pair_left_identity, prop_pair_right_identity, prop_pair_associativity, prop_pair_swap
  , prop_choose_left_identity, prop_choose_right_identity, prop_choose_associativity
  , prop_choose_left_absorbtion, prop_choose_left_catch, prop_choose_idempotent
  , prop_choose_not_commutative
  , prop_step_left_identity, prop_step_right_identity, prop_step_assocaitivity, prop_step_left_anihilation
  ) where


import Preload

import Data.Task
import Data.Task.Input

import System.IO.Unsafe


infix 1 ===, !==, ~=



-- Equivallence ----------------------------------------------------------------


(===) :: Basic a => MonadRef l m => TaskT l m a -> TaskT l m a -> m Bool
t1 === t2 = do
  ( (_, v1), (_, v2) ) <- norm t1 t2
  pure $ v1 == v2


(!==) :: Basic a => MonadRef l m => TaskT l m a -> TaskT l m a -> m Bool
t1 !== t2 = do
  ( (_, v1), (_, v2) ) <- norm t1 t2
  pure $ v1 /= v2


norm :: MonadRef l m => TaskT l m a -> TaskT l m a -> m ( (TaskT l m a, Maybe a), ( TaskT l m a, Maybe a ) )
norm t1 t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  pure $ ( (t1', v1), (t2', v2) )



-- Similarity ------------------------------------------------------------------


(~=) :: Basic a => MonadTrace NotApplicable m => MonadRef l m => TaskT l m a -> TaskT l m a -> m Bool
t1 ~= t2 = do
  ( (t1', v1), (t2', v2) ) <- norm t1 t2
  if v1 == v2 then do
    ok12 <- simulating t1' t2'
    ok21 <- simulating t2' t1'
    pure $ ok12 && ok21
  else
    pure False

  where

    simulating t1' t2' = do
      is1 <- inputs t1'
      is2 <- inputs t2'
      forall is1 $ \i1 -> forany is2 $ \i2 -> progressing t1' i1 t2' i2

    progressing t1' i1 t2' i2
      | i1 ~? i2 = do
          t1'' <- handle t1' i1
          t2'' <- handle t2' i2
          t1'' ~= t2''
      | otherwise =
          pure True



-- Properties ------------------------------------------------------------------

-- Values --


prop_equal_val :: TaskIO Int -> Bool
prop_equal_val t = unsafePerformIO $ do
  x <- value t
  y <- value t
  pure $ x == y


prop_failing_preserved :: TaskIO Int -> Bool
prop_failing_preserved t = unsafePerformIO $ do
  t' <- normalise t
  pure $ failing t == failing t'


-- prop_norm_val :: Task Int -> Bool
-- prop_norm_val t = unsafePerformIO $ do
--   v <- value t
--   t' <- normalise t
--   v' <- value t'
--   pure $ v == v'



-- Monoidal functor --


prop_pair_left_identity :: TaskIO Int -> Bool
prop_pair_left_identity t = unsafePerformIO $
  lift fst (t |&| edit ()) === t


prop_pair_right_identity :: TaskIO Int -> Bool
prop_pair_right_identity t = unsafePerformIO $
  lift snd (edit () |&| t) === t


prop_pair_associativity :: TaskIO Int -> TaskIO Int -> TaskIO Int -> Bool
prop_pair_associativity r s t = unsafePerformIO $
  lift assoc (r |&| (s |&| t)) === (r |&| s) |&| t


prop_pair_swap :: TaskIO Int -> TaskIO Int -> Bool
prop_pair_swap s t = unsafePerformIO $
  lift swap (s |&| t) === t |&| s



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
  fail >>! (\_ -> t) === fail


prop_choose_left_catch :: Int -> TaskIO Int -> Bool
prop_choose_left_catch x t = unsafePerformIO $
  edit x |!| t === edit x


prop_choose_idempotent :: TaskIO Int -> Bool
prop_choose_idempotent t = unsafePerformIO $
  t |!| t === t


prop_choose_not_commutative :: TaskIO Int -> TaskIO Int -> Bool
prop_choose_not_commutative t1 t2 = unsafePerformIO $
  t1 |!| t2 === t2 |!| t1



-- Monad --


prop_step_left_identity :: Int -> TaskIO Int -> Bool
prop_step_left_identity x t = unsafePerformIO $
  edit x >>! (\_ -> t) === t


prop_step_right_identity :: TaskIO Int -> Bool
prop_step_right_identity t = unsafePerformIO $
  t >>! (\y -> edit y) === t


prop_step_assocaitivity :: TaskIO Int -> TaskIO Int -> TaskIO Int -> Bool
prop_step_assocaitivity r s t = unsafePerformIO $
  (r >>! (\_ -> s)) >>! (\_ -> t) === r >>! (\_ -> s >>! (\_ -> t))


prop_step_left_anihilation :: TaskIO Int -> Bool
prop_step_left_anihilation t = unsafePerformIO $
  fail >>! (\_ -> t) === fail



-- Helpers --


assoc :: ( a, ( b, c ) ) -> ( ( a, b ), c )
assoc ( a, ( b, c ) ) = ( ( a, b ), c )
