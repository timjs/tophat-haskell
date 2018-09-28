module Task.Equality


import Task.Internal
import Task.Semantics

import Helpers


%default total
%access export



-- Equivallence ----------------------------------------------------------------


equivallent : MonadRef l m => TaskT m a -> TaskT m a -> m Bool
equivallent {a} t1 t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  let _ = eq a --NOTE: Brings `Eq` in scope for universe type `a`
  pure $ v1 == v2



-- Similarity ------------------------------------------------------------------


similar : MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> TaskT m a -> m Bool
similar {a} t1 t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  let _ = eq a --NOTE: Brings `Eq` in scope for universe type `a`
  if v1 == v2 then do
    ok12 <- sim t1 t2
    ok21 <- sim t2 t1
    pure $ ok12 && ok21
  else
    pure False
where
    sim : MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> TaskT m a -> m Bool
    sim t1 t2 = do
      is1 <- inputs t1
      is2 <- inputs t2
      pairs <- sequence [ bisequence ( handle t1 i1, handle t2 i2 ) | i1 <- is1, i2 <- is2, i2 =~ i1 ]
      results <- sequence $ map (uncurry similar) pairs
      pure $ and results



-- Conform ---------------------------------------------------------------------
