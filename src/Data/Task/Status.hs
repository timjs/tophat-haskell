module Data.Task.Status where

import Data.Task (Task(..), Collaborative(..))


-- Status ----------------------------------------------------------------------

data Status a
  = Failing
  | Looping
  | Stepping
  | Asking
  | Terminating a
  deriving ( Show, Eq, Ord, Functor )

instance Monoidal Status where
  skip = Terminating ()

  Terminating x <&> Terminating y = Terminating ( x, y )
  Failing       <&> _             = Failing
  _             <&> Failing       = Failing
  Looping       <&> _             = Looping
  _             <&> Looping       = Looping
  Stepping      <&> _             = Stepping
  _             <&> Stepping      = Stepping
  Asking        <&> _             = Asking
  _             <&> Asking        = Asking

instance Applicative Status where
  pure  = pureDefault
  (<*>) = applyDefault

{-
instance Applicative Status where
  Terminating f <*> Terminating x = Terminating (f x)
  Terminating _ <*> Failing     = Failing
  Terminating _ <*> Looping     = Looping
  Terminating _ <*> Stepping    = Stepping
  Terminating _ <*> Asking      = Asking
  Failing     <*> _           = Failing
  Looping     <*> _           = Looping
  Stepping    <*> _           = Stepping
  Asking      <*> _           = Asking

  pure = Terminating

instance Monoidal Status
-}

instance Alternative Status where
  empty = Failing

  Terminating x <|> _             = Terminating x
  _             <|> Terminating y = Terminating y
  _             <|> _             = Failing  --XXX


-- Observation -----------------------------------------------------------------

status :: Collaborative r m => Task m a -> m (Status a)
status = \case
  Done v       -> pure (Terminating v)
  Update v     -> pure (Terminating v)
  View v       -> pure (Terminating v)

  Share v      -> pure Terminating -< share v
  Assign _ _   -> pure (Terminating ())
  Change l     -> pure Terminating -< watch l
  Watch l      -> pure Terminating -< watch l

  Pair t1 t2   -> pure (<&>) -< status t1 -< status t2
  Choose t1 t2 -> pure (<|>) -< status t1 -< status t2
  Trans f t    -> pure (map f) -< status t

  Fail         -> pure Failing

  Forever _    -> pure Looping

  Step _ _     -> pure Stepping
  Pick _       -> pure Stepping

  Enter        -> pure Asking

{-
instance Eq a => Eq (Task m a) where
  -- We can only equate values in a `Done` iff their contents are `Eq`.
  Done v1        == Done v2        = v1 == v2
  -- Empty editors are equal when their types match.
  Enter          == Enter          = True
  -- Editors are equal when their contents are equal...
  Update v1      == Update v2      = v1 == v2
  -- ...same holds for view only editors.
  View v1        == View v2        = v1 == v2
  -- FIXME: What to do with picks?
  Pick ts1       == Pick ts2       = undefined
  -- Pairs are equal when their parts are equal...
  Pair t11 t12   == Pair t21 t22   = t11 == t21 && t12 == t22
  -- ...same holds for choices.
  Choose t11 t12 == Choose t21 t22 = t11 == t21 && t12 == t22
  -- Two `Fail`s are always equal when the types match.
  Fail           == Fail           = True
  -- FIXME: What to do with transformations?
  Trans f1 t1    == Trans f2 t2    = undefined
  -- Steps can never be equal, comparing continuations is not possible.
  Step _ _       == Step _ _       = False
  -- Loops are equal when their parts are equal.
  Forever t1     == Forever t2     = t1 == t2
  -- Shares of equal values are equal.
  Share v1       == Share v2       = v1 == v2
  Assign v1 l1   == Assign v2 l2   = v1 == v2 && l1 == l2
  -- Shared editors are equal when they watch the same location...
  Watch l1       == Watch l2       = l1 == l2
  -- ...same holds for view only shared editors.
  Change l1      == Change l2      = l1 == l2
-}
