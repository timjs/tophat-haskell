module Data.Task.Status where

import Data.Task (Collaborative (..), Task (..))

-- Definition ------------------------------------------------------------------

-- | The status of a `Task a` is defined to be one of four options:
data Status a
  = Producing a -- it is producing a value of type `a` (i.e. valued and shared editors)
  | Asking -- it is asking for a value of type `a` (i.e. empty editors)
  | Failing -- it is failing, and always will be failing (i.e. the failing task)
  | Stepping -- it is still stepping, and we do not know how it will end
  deriving (Show, Functor)

-- Equality --------------------------------------------------------------------

-- | Status observations _of the same type_ are equal in only three cases:
instance Eq a => Eq (Status a) where
  Producing x == Producing y = x == y -- when the values produced are equal
  Asking == Asking = True -- when both ask for a value
  Failing == Failing = True -- when both are `Failing`
  _ == _ = False -- in any other case, tasks are looping or stepping and we do not know how things will end.

-- Combinations ----------------------------------------------------------------

instance Monoidal Status where
  skip = Producing ()

  Producing x >< Producing y = Producing (x, y)
  Asking >< Asking = Asking
  Failing >< Failing = Failing
  _ >< _ = Stepping

instance Applicative Status where
  pure = pureDefault
  (<*>) = applyDefault

instance Alternative Status where
  empty = Stepping

  Producing x <|> _ = Producing x
  _ <|> Producing y = Producing y
  _ <|> _ = Stepping

-- Observation -----------------------------------------------------------------

status :: Collaborative r m => Task m a -> m (Status a)
status = \case
  -- Valued editors are `Producing` a value... --
  Done v -> pure (Producing v)
  Update v -> pure (Producing v)
  View v -> pure (Producing v)
  -- ...as do shared editors. --
  Share v -> pure Producing -< share v
  Assign _ _ -> pure (Producing ())
  Change l -> pure Producing -< watch l
  Watch l -> pure Producing -< watch l
  -- Unvalued editors are `Asking` for a value. --
  Enter -> pure Asking
  -- Fail is obviously `Failing`... --
  Fail -> pure Failing
  -- Combinations pair or choose one of the possibilities. --
  Pair t1 t2 -> pure (><) -< status t1 -< status t2
  Choose t1 t2 -> pure (<|>) -< status t1 -< status t2
  Trans f t -> pure (map f) -< status t
  -- From the rest we do not know anything. --
  Pick _ -> pure Stepping
  Step _ _ -> pure Stepping
  Forever _ -> pure Stepping
