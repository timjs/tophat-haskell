module Data.Task.Status where

import Data.Task (Task(..), Collaborative(..))


-- Reason ----------------------------------------------------------------------

data Reason
  = Failing
  | Looping
  | Stepping
  | Asking
  deriving ( Show, Eq, Ord )

instance Pretty Reason where
  pretty = viaShow

instance Semigroup Reason where
  Failing  <> _        = Failing
  _        <> Failing  = Failing
  Looping  <> _        = Looping
  _        <> Looping  = Looping
  Stepping <> _        = Stepping
  _        <> Stepping = Stepping
  Asking   <> Asking   = Asking


-- Status ----------------------------------------------------------------------

data Status a where
  Terminating    :: a -> Status a
  Nonterminating :: Reason -> Status a
  Pairing        :: Status a -> Status b -> Status ( a, b )

-- deriving instance Show a => Show (Status a)
-- deriving instance Eq a => Eq (Status a)

instance Pretty a => Pretty (Status a) where
  pretty = \case
    Terminating x    -> sep [ "Terminating", pretty x ]
    Nonterminating r -> sep [ "Nonterminating", pretty r ]
    Pairing _ _      -> undefined

instance Functor Status where
  fmap f = \case
    Terminating x    -> Terminating (f x)
    Nonterminating r -> Nonterminating r
    Pairing x y      -> map f <| x <&> y

instance Monoidal Status where
  skip = Terminating ()

  Terminating x1    <&> Terminating x2    = Terminating ( x1, x2 )
  Nonterminating r1 <&> Nonterminating r2 = Nonterminating (r1 <> r2)
  Pairing x1 y1     <&> Pairing x2 y2     = (x1 <&> y1) <&> (x2 <&> y2)
  s1                <&> s2                = Pairing s1 s2

instance Applicative Status where
  pure  = pureDefault
  (<*>) = applyDefault

instance Alternative Status where
  empty = Nonterminating Failing

  Terminating x1    <|> _                 = Terminating x1
  _                 <|> Terminating x2    = Terminating x2
  Nonterminating r1 <|> Nonterminating r2 = Nonterminating (r1 <> r2)


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

  Fail         -> pure (Nonterminating Failing)

  Forever _    -> pure (Nonterminating Looping)

  Step _ _     -> pure (Nonterminating Stepping)
  Pick _       -> pure (Nonterminating Stepping)

  Enter        -> pure (Nonterminating Asking)

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
