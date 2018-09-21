module Task.Equality


import Task.Internal
import Task.Semantics


%default total
%access export



-- Equivallence ----------------------------------------------------------------


equivallent : Eq (typeOf a) => MonadRef l m => TaskT m a -> TaskT m a -> m Bool
equivallent t1 t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  pure $ v1 == v2



-- Similarity ------------------------------------------------------------------


similar : Eq (typeOf a) => MonadRef l m => TaskT m a -> TaskT m a -> m Bool
similar t1 t2 = do
  t1' <- normalise t1
  t2' <- normalise t2
  v1 <- value t1'
  v2 <- value t2'
  if v1 == v2 then
    pure ?h
  else
    pure False
