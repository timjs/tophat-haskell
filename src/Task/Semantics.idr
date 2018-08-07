module Task

import Control.Monad.Ref

import Task.Internal
import Task.Universe
import Task.Event
import Helpers

%default total
%access export

%hide Language.Reflection.Elab.Tactics.normalise


-- Errors ----------------------------------------------------------------------

data NotApplicable
  = CouldNotChange
  | CouldNotFind Label
  | CouldNotContinue
  | CouldNotHandle Event

Show NotApplicable where
  show (CouldNotChange)   = "Could not change value because types do not match"
  show (CouldNotFind l)   = "Could not find label `" ++ l ++ "`"
  show (CouldNotContinue) = "Could not continue"
  show (CouldNotHandle e) = "Could not handle event `" ++ show e ++ "`"


-- Showing ---------------------------------------------------------------------

ui : MonadRef l m => Show (typeOf a) => Task m a -> m String
ui (Edit (Just x))       = pure $ "□(" ++ show x ++ ")"
ui (Edit Nothing)        = pure $ "□(_)"
ui (Watch loc)           = pure $ "■(" ++ show !(deref loc) ++ ")"
ui (All left right)      = pure $ !(ui left) ++ "   ⋈   " ++ !(ui right)
ui (Any left right)      = pure $ !(ui left) ++ "   ◆   " ++ !(ui right)
ui (One left right) with ( delabel left, delabel right )
  | ( One _ _, One _ _ ) = pure $                 !(ui left) ++ " ◇ " ++ !(ui right)
  | ( One _ _, _       ) = pure $                 !(ui left) ++ " ◇ " ++ fromMaybe "…" (label right)
  | ( _,       One _ _ ) = pure $ fromMaybe "…" (label left) ++ " ◇ " ++ !(ui right)
  | ( _,       _       ) = pure $ fromMaybe "…" (label left) ++ " ◇ " ++ fromMaybe "…" (label right)
ui (Fail)                = pure $ "↯"
ui (Then this cont)      = pure $ !(ui this) ++ " ▶…"
ui (Next this cont)      = pure $ !(ui this) ++ " ▷…"
ui (Label l this)        = pure $ l ++ " # " ++ !(ui this)


-- Helpers ---------------------------------------------------------------------

value : MonadRef l m => Task m a -> m (Maybe (typeOf a))
value (Edit val)       = pure $ val
value (Watch loc)      = pure $ Just !(deref loc)
value (All left right) = pure $ !(value left) <&> !(value right)
value (Any left right) = pure $ !(value left) <|> !(value right)
value (Label _ this)   = value this
-- The rest never has a value because:
--   * `One` and `Next` need to wait for an user choice
--   * `Fail` runs forever and doesn't produce a value
--   * `Then` transforms values to another type
value _                = pure $ Nothing

choices : Task m a -> List Path
choices (One left right) =
  --XXX: No with-block possible?
  case ( delabel left, delabel right ) of
    ( Fail, Fail  ) => []
    ( left, Fail  ) => map GoLeft (GoHere :: choices left)
    ( Fail, right ) => map GoRight (GoHere :: choices right)
    ( left, right ) => map GoLeft (GoHere :: choices left) ++ map GoRight (GoHere :: choices right)
choices _           = []

events : MonadRef l m => Task m a -> m (List Event)
events (Edit {a} _)     = pure $ [ ToHere (Change (defaultOf a)), ToHere Clear ]
events (Watch {a} _)    = pure $ [ ToHere (Change (defaultOf a)) ]
events (All left right) = pure $ map ToLeft !(events left) ++ map ToRight !(events right)
events (Any left right) = pure $ map ToLeft !(events left) ++ map ToRight !(events right)
events this@(One _ _)   = pure $ map (ToHere . PickAt) (labels this) ++ map (ToHere . Pick) (choices this)
events (Fail)           = pure $ []
events (Then this _)    = events this
events (Next this next) = do
    Just v <- value this | Nothing => pure []
    pure $ map ToHere (go (next v)) ++ !(events this)
  where
    go : Task m a -> List Action
    go task with ( delabel task )
      | One _ _ = map (Continue . Just) $ labels task
      | Fail    = []
      | _       = [ Continue Nothing ]
events (Label _ this)   = events this


-- Normalisation ---------------------------------------------------------------

normalise : MonadRef l m => Task m a -> m (Task m a)

-- Step --
normalise (Then this cont) = do
  this_new <- normalise this
  case !(value this_new) of
    Nothing => pure $ Then this_new cont
    Just v  =>
      --FIXME: should we use normalise here instead of just eval?
      case cont v of
        Fail => pure $ Then this_new cont
        next => normalise next

-- Evaluate --
normalise (All left right) = do
  left_new <- normalise left
  right_new <- normalise right
  pure $ All left_new right_new

normalise (Any left right) = do
  left_new <- normalise left
  right_new <- normalise right
  case !(value left_new) of
    Just _  => pure $ left_new
    Nothing =>
      case !(value right_new) of
        Just _  => pure $ right_new
        Nothing => pure $ Any left_new right_new

normalise (Next this cont) = do
  this_new <- normalise this
  pure $ Next this_new cont

-- Label --
normalise (Label l this) with ( keeper this )
  | False = normalise this
  | True  = do
      this_new <- normalise this
      pure $ Label l this_new

-- Values --
normalise task = do
  pure $ task


{- Event handling --------------------------------------------------------------

--FIXME: fix totallity...
-- Edit --
handle  : MonadRef l m => Task m a -> Event -> Either NotApplicable (m (Task m a))
handle' : MonadRef l m => Task m a -> Event -> m (Either NotApplicable (Task m a))
handle (Edit _) (ToHere Clear) state =
  ok ( Edit Nothing, state )
handle (Edit {a} val) (ToHere (Change {b} val_new)) state with (decEq b a)
  handle (Edit _) (ToHere (Change val_new)) state         | Yes Refl = ok ( Edit (Just val_new), state )
  handle (Edit val) (ToHere (Change val_new)) _           | No _     = throw CouldNotChange
handle Watch (ToHere (Change {b} val_new)) state with (decEq b StateTy)
  handle Watch (ToHere (Change val_new)) _       | Yes Refl = ok ( Watch, val_new )
  handle Watch (ToHere (Change val_new)) _       | No _     = throw CouldNotChange
-- Pass to this --
handle (Then this cont) event state = do
  -- Pass the event to this
  ( this_new, state_new ) <- handle this event state
  ok ( Then this_new cont, state_new )
-- Pass to left or right --
handle (All left right) (ToLeft event) state = do
  -- Pass the event to left
  ( left_new, state_new ) <- handle left event state
  ok ( All left_new right, state_new )
handle (All left right) (ToRight event) state = do
  -- Pass the event to right
  ( right_new, state_new ) <- handle right event state
  ok ( All left right_new, state_new )
handle (Any left right) (ToLeft event) state = do
  -- Pass the event to left
  ( left_new, state_new ) <- handle left event state
  ok ( Any left_new right, state_new )
handle (Any left right) (ToRight event) state = do
  -- Pass the event to right
  ( right_new, state_new ) <- handle right event state
  ok ( Any left right_new, state_new )
-- Interact
handle task@(One _ _) (ToHere (PickAt l)) state =
  case find l task of
    Nothing => throw $ CouldNotFind l
    Just p  => handle task (ToHere (Pick p)) state
handle (One left _) (ToHere (Pick (GoLeft p))) state =
  -- Go left
  handle (delabel left) (ToHere (Pick p)) state
handle (One _ right) (ToHere (Pick (GoRight p))) state =
  -- Go right
  handle (delabel right) (ToHere (Pick p)) state
handle task (ToHere (Pick GoHere)) state =
  -- Go here
  ok ( task, state )
handle task@(Next this cont) (ToHere (Continue Nothing)) state =
  -- When pressed continue rewrite to an internal step
  ok ( Then this cont, state )
handle task@(Next this cont) (ToHere (Continue (Just l))) state =
  case value this state of
    Nothing => throw CouldNotContinue
    Just v  =>
      let
        next = cont v
      in
      case find l next of
        Nothing => throw $ CouldNotFind l
        Just p  => handle next (ToHere (Pick p)) state
handle (Next this cont) event state = do
  -- Pass the event to this
  ( this_new, state_new ) <- handle this event state
  ok ( Next this_new cont, state_new )
-- Label
handle (Label l this) event state with ( keeper this )
  | False = handle this event state
  | True = do
      ( this_new, state_new ) <- handle this event state
      ok ( Label l this_new, state_new )
-- Rest
handle task event state =
  -- Case `Fail`: Evaluation continues indefinitely
  -- Cases `Get` and `Put`: This case can't happen, it is already evaluated by `normalise`
  throw $ CouldNotHandle event

drive : Task a -> Event -> State -> Either NotApplicable ( Task a, State )
drive task event state =
  uncurry normalise <$> handle task event state
-- do
--   ( task_new, state_new ) <- handle task event state
--   ok $ normalise task_new state_new

init : Task a -> ( Task a, State )
init = flip normalise []

-}
