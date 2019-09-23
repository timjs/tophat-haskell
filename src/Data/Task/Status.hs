module Data.Task.Status where

import Data.Task (Task(..), Collaborative(..))


-- Definition ------------------------------------------------------------------

-- | The status of a `Task a` is defined to be one of four options:
data Status a
  = Producing a  -- ^ it is producing a value of type `a` (i.e. valued and shared editors)
  | Asking       -- ^ it is asking for a value of type `a` (i.e. empty editors)
  | Failing      -- ^ it is failing, and always will be failing (i.e. the failing task)
  | Looping      -- ^ it is looping due to a forever running task or a subtask which is looping or failing
  | Stepping     -- ^ it is still stepping, and we do not know how it will end
  deriving ( Show, Functor )


-- Equality --------------------------------------------------------------------

-- | Status observations _of the same type_ are equal in only three cases:
instance Eq a => Eq (Status a) where
  Producing x == Producing y = x == y  -- ^ when the values produced are equal
  Asking      == Asking      = True    -- ^ when both ask for a value
  Failing     == Failing     = True    -- ^ when both are `Failing`
  _           == _           = False
  -- ^ in any other case, tasks are looping or stepping and we do not know how things will end.


-- Combinations ----------------------------------------------------------------

instance Monoidal Status where
  skip = Producing ()

  -- | The parallel combination of two `Status`es is simple when both tasks are `Producing` a value, then we create a pair.
  Producing x <&> Producing y = Producing ( x, y )
  -- | When both tasks are `Asking` for a value, we could as well ask for a pair immediately.
  Asking      <&> Asking      = Asking
  -- | When both tasks are failing, we could as well fail immediately.
  Failing     <&> Failing     = Failing
  -- | When one of the two tasks is looping, the combination is looping.
  Looping     <&> _           = Looping
  _           <&> Looping     = Looping
  -- | Otherwise we are still stepping, and do not know how things will end.
  _           <&> _           = Stepping

instance Applicative Status where
  pure  = pureDefault
  (<*>) = applyDefault

instance Alternative Status where
  -- | `Stepping` is the status we know the least about.
  empty = Stepping

  -- | The alternative combination of two `Status`es picks the first to have a value.
  -- | It doesn't matter if one is looping or not.
  Producing x <|> _           = Producing x
  _           <|> Producing y = Producing y
  -- | Otherwise, when one of the two tasks is looping, we know we will loop while making a choice.
  Looping     <|> _           = Looping
  _           <|> Looping     = Looping
  -- | Otherwise we are still stepping, and do not know which choice to make.
  _           <|> _           = Stepping


-- Observation -----------------------------------------------------------------

status :: Collaborative r m => Task m a -> m (Status a)
status = \case
  -- | Valued editors are `Producing` a value...
  Done v       -> pure (Producing v)
  Update v     -> pure (Producing v)
  View v       -> pure (Producing v)

  -- | ...as do shared editors.
  Share v      -> pure Producing -< share v
  Assign _ _   -> pure (Producing ())
  Change l     -> pure Producing -< watch l
  Watch l      -> pure Producing -< watch l

  -- | Unvalued editors are `Asking` for a value.
  Enter        -> pure Asking

  -- | Fail is obviously `Failing`...
  Fail         -> pure Failing

  -- | ...and loops are `Looping`.
  Forever _    -> pure Looping

  -- | Combinations pair or choose one of the possibilities.
  Pair t1 t2   -> pure (<&>) -< status t1 -< status t2
  Choose t1 t2 -> pure (<|>) -< status t1 -< status t2
  Trans f t    -> pure (map f) -< status t

  -- | From the rest we do not know anything.
  Pick _       -> pure Stepping
  Step _ _     -> pure Stepping
