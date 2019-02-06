module Data.Task.Equality
  ( (===), (!==) --, (~=)
  , prop_value_equal, prop_failing_preserved, prop_value_preserved
  , prop_pair_left_identity, prop_pair_right_identity, prop_pair_associativity, prop_pair_swap
  , prop_choose_left_identity, prop_choose_right_identity, prop_choose_associativity
  , prop_choose_left_catch, prop_choose_idempotent
  , prop_choose_commutative, prop_choose_distributive
  , prop_step_left_identity, prop_step_right_identity, prop_step_assocaitivity, prop_step_left_anihilation, prop_step_left_absorption
  ) where


import Preload

import Data.Task
-- import Data.Task.Input


infix 1 ===, !== --, ~=



-- Equivallence ----------------------------------------------------------------


(===) :: Basic a => Task a -> Task a -> Bool
t1 === t2 =
  v1 == v2 && s1 == s2
  where
    ( v1, s1 ) = norm t1
    ( v2, s2 ) = norm t2


(!==) :: Basic a => Task a -> Task a -> Bool
t1 !== t2 = not $ t1 === t2


norm :: Task a -> ( Maybe a, Heap )
norm t = runMem $ do
  t' <- normalise t
  v <- value t'
  pure v



{- Similarity ------------------------------------------------------------------


(~=) ::
  Basic a => MonadTrace NotApplicable m => MonadRef l m =>
  TaskT l m a -> TaskT l m a ->
  m Bool
t1 ~= t2 =
  --NOTE: Check bisimulation with a maximum recursion depth
  go 5 t1 t2

  where

    go 0 _ _ =
      pure True
    go maxIter t1 t2 = do
      ( (t1', v1), (t2', v2) ) <- norm t1 t2
      if v1 == v2 then do
        ok12 <- simulating maxIter t1' t2'
        ok21 <- simulating maxIter t2' t1'
        pure $ ok12 && ok21
      else
        pure False

    simulating maxIter t1' t2' = do
      is1 <- map fill <$> inputs t1'
      is2 <- map fill <$> inputs t2'
      forall is1 $ \i1 -> forany is2 $ \i2 -> progressing maxIter t1' i1 t2' i2

    progressing ::
      Int -> TaskT l m a -> Input Action -> TaskT l m a -> Input Action ->
      m Bool
    progressing maxIter t1' i1 t2' i2 = do
      t1'' <- handle t1' i1
      t2'' <- handle t2' i2
      go (pred maxIter) t1'' t2''

-}



-- Properties ------------------------------------------------------------------

-- Values --


prop_value_equal :: Task Int -> Bool
prop_value_equal t = evalMem $ do
  x <- value t
  y <- value t
  pure $ x == y


prop_failing_preserved :: Task Int -> Bool
prop_failing_preserved t = evalMem $ do
  t' <- normalise t
  pure $ failing t == failing t'

{-
  Counter example:

    Edit 42 >>= \x -> Edit x
-}
prop_value_preserved :: Task Int -> Bool
prop_value_preserved t = evalMem $ do
  v <- value t
  t' <- normalise t
  v' <- value t'
  pure $ v == v'


{- Equivallences --


prop_choose_is_edit :: Task Int -> Task Int -> Bool
prop_choose_is_edit s t =  do
  s -??- t ~= (enter >>- \b -> if b then s else t)


prop_continue_is_edit :: Task Int -> Task Int -> Bool
prop_continue_is_edit s t =  do
  (s >>? \_ -> t) ~= (enter -&&- s >>- \( (), _ ) -> t)


-}


-- Monoidal functor --


prop_pair_left_identity :: Task Int -> Bool
prop_pair_left_identity t =
  lift fst (t -&&- edit ()) === t


prop_pair_right_identity :: Task Int -> Bool
prop_pair_right_identity t =
  lift snd (edit () -&&- t) === t


prop_pair_associativity :: Task Int -> Task Int -> Task Int -> Bool
prop_pair_associativity r s t =
  lift assoc (r -&&- (s -&&- t)) === (r -&&- s) -&&- t


prop_pair_swap :: Task Int -> Task Int -> Bool
prop_pair_swap s t =
  lift swap (s -&&- t) === t -&&- s



-- Alternative functor --


type Choose = Task Int -> Task Int -> Task Int

prop_choose_left_identity :: Choose -> Task Int -> Bool
prop_choose_left_identity (-?-) t =
  failure -?- t === t


prop_choose_right_identity :: Choose -> Task Int -> Bool
prop_choose_right_identity (-?-) t =
  t -?- failure === t


prop_choose_associativity ::
  Choose -> Task Int -> Task Int -> Task Int ->
  Bool
prop_choose_associativity (-?-) r s t =
  r -?- (s -?- t) === (r -?- s) -?- t


prop_choose_left_catch :: Choose -> Int -> Task Int -> Bool
prop_choose_left_catch (-?-) x t =
  edit x -?- t === edit x


prop_choose_idempotent :: Choose -> Task Int -> Bool
prop_choose_idempotent (-?-) t =
  t -?- t === t


prop_choose_commutative :: Choose -> Task Int -> Task Int -> Bool
prop_choose_commutative (-?-) t1 t2 =
  t1 -?- t2 === t2 -?- t1


prop_choose_distributive ::
  Choose -> Task Int -> Task Int -> Task Int ->
  Bool
prop_choose_distributive (-?-) r s t =
  (r -?- s) >>- (\_ -> t) === (r >>- \_ -> t) -?- (s >>- \_ -> t)



-- Monad --


type Bind = Task Int -> (Int -> Task Int) -> Task Int


prop_step_left_identity :: Bind -> Int -> Task Int -> Bool
prop_step_left_identity (>-) x t =
  edit x >- (\_ -> t) === t


prop_step_right_identity :: Bind -> Task Int -> Bool
prop_step_right_identity (>-) t =
  t >- (\y -> edit y) === t


prop_step_assocaitivity :: Bind -> Task Int -> Task Int -> Task Int -> Bool
prop_step_assocaitivity (>-) r s t =
  (r >- (\_ -> s)) >- (\_ -> t) === r >- (\_ -> s >- (\_ -> t))


prop_step_left_anihilation :: Bind -> Task Int -> Bool
prop_step_left_anihilation (>-) t =
  failure >- (\_ -> t) === failure


prop_step_left_absorption :: Bind -> Task Int -> Bool
prop_step_left_absorption (>-) t =
  failure >- (\_ -> t) === failure




-- Helpers --


assoc :: ( a, ( b, c ) ) -> ( ( a, b ), c )
assoc ( a, ( b, c ) ) = ( ( a, b ), c )
