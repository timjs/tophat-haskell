module Task


import Control.Monad.Trace
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


ui : MonadRef l m => Show (typeOf a) => TaskT m a -> m String
ui (Edit (Just x))       = pure $ "□(" ++ show x ++ ")"
ui (Edit Nothing)        = pure $ "□(_)"
ui (Watch loc)           = pure $ "■(" ++ show !(deref loc) ++ ")"
ui (All left rght)       = pure $ !(ui left) ++ "   ⋈   " ++ !(ui rght)
ui (Any left rght)       = pure $ !(ui left) ++ "   ◆   " ++ !(ui rght)
ui (One left rght) with ( delabel left, delabel rght )
  | ( One _ _, One _ _ ) = pure $                 !(ui left) ++ " ◇ " ++ !(ui rght)
  | ( One _ _, _       ) = pure $                 !(ui left) ++ " ◇ " ++ fromMaybe "…" (label rght)
  | ( _,       One _ _ ) = pure $ fromMaybe "…" (label left) ++ " ◇ " ++ !(ui rght)
  | ( _,       _       ) = pure $ fromMaybe "…" (label left) ++ " ◇ " ++ fromMaybe "…" (label rght)
ui (Fail)                = pure $ "↯"
ui (Then this cont)      = pure $ !(ui this) ++ " ▶…"
ui (Next this cont)      = pure $ !(ui this) ++ " ▷…"
ui (Label l this)        = pure $ l ++ " # " ++ !(ui this)



-- Helpers ---------------------------------------------------------------------


value : MonadRef l m => TaskT m a -> m (Maybe (typeOf a))
value (Edit val)      = pure $ val
value (Watch loc)     = pure $ Just !(deref loc)
value (All left rght) = pure $ !(value left) <&> !(value rght)
value (Any left rght) = pure $ !(value left) <|> !(value rght)
value (Label _ this)  = value this
-- The rest never has a value because:
--   * `One` and `Next` need to wait for an user choice
--   * `Fail` runs forever and doesn't produce a value
--   * `Then` transforms values to another type
value _               = pure $ Nothing


choices : TaskT m a -> List Path
choices (One left rght) =
  --XXX: No with-block possible?
  case ( delabel left, delabel rght ) of
    ( Fail, Fail ) => []
    ( left, Fail ) => map GoLeft (GoHere :: choices left)
    ( Fail, rght ) => map GoRight (GoHere :: choices rght)
    ( left, rght ) => map GoLeft (GoHere :: choices left) ++ map GoRight (GoHere :: choices rght)
choices _          = []


events : MonadRef l m => TaskT m a -> m (List Event)
events (Edit {a} _)     = pure $ [ ToHere (Change (defaultOf a)), ToHere Clear ]
events (Watch {a} _)    = pure $ [ ToHere (Change (defaultOf a)) ]
events (All left rght)  = pure $ map ToLeft !(events left) ++ map ToRight !(events rght)
events (Any left rght)  = pure $ map ToLeft !(events left) ++ map ToRight !(events rght)
events this@(One _ _)   = pure $ map (ToHere . PickAt) (labels this) ++ map (ToHere . Pick) (choices this)
events (Fail)           = pure $ []
events (Then this _)    = events this
events (Next this next) = do
    Just v <- value this | Nothing => pure []
    pure $ map ToHere (go (next v)) ++ !(events this)
  where
    go : TaskT m a -> List Action
    go task with ( delabel task )
      | One _ _ = map (Continue . Just) $ labels task
      | Fail    = []
      | _       = [ Continue Nothing ]
events (Label _ this)   = events this



-- Normalisation ---------------------------------------------------------------


normalise : MonadRef l m => TaskT m a -> m (TaskT m a)

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
normalise (All left rght) = do
  left_new <- normalise left
  rght_new <- normalise rght
  pure $ All left_new rght_new

normalise (Any left rght) = do
  left_new <- normalise left
  rght_new <- normalise rght
  case !(value left_new) of
    Just _  => pure $ left_new
    Nothing =>
      case !(value rght_new) of
        Just _  => pure $ rght_new
        Nothing => pure $ Any left_new rght_new

normalise (Next this cont) = do
  this_new <- normalise this
  pure $ Next this_new cont

-- Label --
normalise (Label l this) with ( keeper this )
  | False = normalise this
  | True  = do
      this_new <- normalise this
      pure $ Label l this_new

-- Lift --
normalise (Lift a) = do
  x <- a
  pure $ Edit (Just x)

-- Values --
normalise task = do
  pure $ task


init : MonadRef l m => TaskT m a -> m (TaskT m a)
init = normalise



-- Event handling --------------------------------------------------------------


--FIXME: fix totallity...
handle : MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Event -> m (TaskT m a)

-- Edit --
handle (Edit _) (ToHere Clear) =
  pure $ Edit Nothing

handle (Edit {a} val) (ToHere (Change {b} val_new)) with (decEq b a)
  handle (Edit _)   (ToHere (Change val_new))       | Yes Refl = pure $ Edit (Just val_new)
  handle (Edit val) (ToHere (Change _))             | No _     = trace CouldNotChange $ Edit val

handle (Watch {a} loc) (ToHere (Change {b} val_new)) with (decEq b a)
  handle (Watch loc) (ToHere (Change val_new))       | Yes Refl = do
    loc := val_new
    pure $ Watch loc
  handle (Watch loc) (ToHere (Change _))             | No _     = trace CouldNotChange $ Watch loc

-- Pass to this --
handle (Then this cont) event = do
  -- Pass the event to this
  this_new <- handle this event
  pure $ Then this_new cont

-- Pass to left or rght --
handle (All left rght) (ToLeft event) = do
  -- Pass the event to left
  left_new <- handle left event
  pure $ All left_new rght

handle (All left rght) (ToRight event) = do
  -- Pass the event to rght
  rght_new <- handle rght event
  pure $ All left rght_new

handle (Any left rght) (ToLeft event) = do
  -- Pass the event to left
  left_new <- handle left event
  pure $ Any left_new rght

handle (Any left rght) (ToRight event) = do
  -- Pass the event to rght
  rght_new <- handle rght event
  pure $ Any left rght_new

-- Interact --
handle task@(One _ _) (ToHere (PickAt l)) =
  case find l task of
    Nothing => trace (CouldNotFind l) task
    Just p  => handle task (ToHere (Pick p))

handle (One left _) (ToHere (Pick (GoLeft p))) =
  -- Go left
  handle (delabel left) (ToHere (Pick p))

handle (One _ rght) (ToHere (Pick (GoRight p))) =
  -- Go rght
  handle (delabel rght) (ToHere (Pick p))

handle task (ToHere (Pick GoHere)) =
  -- Go here
  pure $ task

handle task@(Next this cont) (ToHere (Continue Nothing)) =
  -- When pressed continue rewrite to an internal step
  pure $ Then this cont

handle task@(Next this cont) (ToHere (Continue (Just l))) =
  case !(value this) of
    Nothing => trace CouldNotContinue task
    Just v  =>
      let
        next = cont v
      in
      case find l next of
        Nothing => trace (CouldNotFind l) task
        Just p  => handle next (ToHere (Pick p))

handle (Next this cont) event = do
  -- Pass the event to this
  this_new <- handle this event
  pure $ Next this_new cont

-- Label --
handle (Label l this) event with ( keeper this )
  | False = handle this event
  | True = do
      this_new <- handle this event
      pure $ Label l this_new

-- Rest --
handle task event =
  -- Case `Fail`: Evaluation continues indefinitely
  trace (CouldNotHandle event) task


drive : MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Event -> m (TaskT m a)
drive task event =
  handle task event >>= normalise



-- Running ---------------------------------------------------------------------


public export
Task : Ty -> Type
Task = TaskT IO


run : Task a -> Event -> IO (Task a)
run = drive
