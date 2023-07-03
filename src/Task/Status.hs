module Task.Status where

{-
import Task (Collaborative (..), Task (..))

---- Definition ----------------------------------------------------------------

-- | The status of a `Task a` is defined to be one of four options:
data Status a
  = Producing a -- it is producing a value of type `a` (i.e. valued and shared editors)
  | Asking -- it is asking for a value of type `a` (i.e. empty editors)
  | Failing -- it is failing, and always will be failing (i.e. the failing task)
  | Stepping -- it is still stepping, and we do not know how it will end
  deriving (Show, Functor)

---- Equality ------------------------------------------------------------------

-- | Status observations _of the same type_ are equal in only three cases:
instance Eq a => Eq (Status a) where
  Producing x == Producing y = x == y -- when the values produced are equal
  Asking == Asking = True -- when both ask for a value
  Failing == Failing = True -- when both are `Failing`
  _ == _ = False -- in any other case, tasks are looping or stepping and we do not know how things will end.

---- Combinations --------------------------------------------------------------

instance Monoidal Status where
  skip = Producing ()

  Producing x >< Producing y = Producing (x, y)
  Asking >< Asking = Asking
  Failing >< Failing = Failing
  _ >< _ = Stepping

instance Applicative Status where
  done = pureDefault
  (<*>) = applyDefault

instance Alternative Status where
  empty = Stepping

  Producing x <|> _ = Producing x
  _ <|> Producing y = Producing y
  _ <|> _ = Stepping

---- Observation ---------------------------------------------------------------

status :: Collaborative r m => Task m a -> m (Status a)
status = \case
  -- Valued editors are `Producing` a value... --
  Lift v -> done (Producing v)
  Update v -> done (Producing v)
  View v -> done (Producing v)
  -- ...as do shared editors. --
  Share v -> done Producing -<< share v
  Assign _ _ -> done (Producing ())
  Change l -> done Producing -<< watch l
  Watch l -> done Producing -<< watch l
  -- Unvalued editors are `Asking` for a value. --
  Enter -> done Asking
  -- Fail is obviously `Failing`... --
  Fail -> done Failing
  -- Combinations pair or choose one of the possibilities. --
  Pair t1 t2 -> done (><) -<< status t1 -<< status t2
  Choose t1 t2 -> done (<|>) -<< status t1 -<< status t2
  Trans f t -> done (map f) -<< status t
  -- From the rest we do not know anything. --
  Option _ -> done Stepping
  Step _ _ -> done Stepping
  Forever _ -> done Stepping
-}
