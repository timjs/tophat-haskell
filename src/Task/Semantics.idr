module Task


import Control.Monad.Trace

import Task.Internal

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
  show (CouldNotContinue) = "Could not continue because there is no value to continue with"
  show (CouldNotHandle e) = "Could not handle event `" ++ show e ++ "`"



-- Showing ---------------------------------------------------------------------


ui : MonadRef l m => Show (typeOf a) => TaskT m a -> m String
ui (Edit (Just x))       = pure $ "□(" ++ show x ++ ")"
ui (Edit Nothing)        = pure $ "□(_)"
ui (Store loc)           = pure $ "■(" ++ show !(deref loc) ++ ")"
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
ui (Label l this)        = pure $ l ++ ":\n\t" ++ !(ui this)
ui (Lift _)              = pure $ "<lift>"



-- Observations ----------------------------------------------------------------


value : MonadRef l m  => TaskT m a -> m (Maybe (typeOf a))
value (Edit val)      = pure $ val
value (Store loc)     = pure $ Just !(deref loc)
value (All left rght) = pure $ !(value left) <&> !(value rght)
value (Any left rght) = pure $ !(value left) <|> !(value rght)
value (One _ _)       = pure $ Nothing
value (Fail)          = pure $ Nothing
value (Then _ _)      = pure $ Nothing
value (Next _ _)      = pure $ Nothing
value (Label _ this)  = value this
value (Lift _)        = pure $ Nothing


choices : TaskT m a -> List Path
choices (One left rght) =
  --XXX: No with-block possible?
  case ( delabel left, delabel rght ) of
    ( Fail, Fail ) => []
    ( left, Fail ) => map GoLeft (GoHere :: choices left)
    ( Fail, rght ) => map GoRight (GoHere :: choices rght)
    ( left, rght ) => map GoLeft (GoHere :: choices left) ++ map GoRight (GoHere :: choices rght)
choices _          = []


inputs : MonadRef l m => TaskT m a -> m (List Event)
inputs (Edit {a} _) with (isBasic a)
  | Yes p               = pure $ [ ToHere (Change {c = a} Nothing), ToHere Empty ]
  | No _                = pure $ [ ToHere Empty ]
inputs (Store {b} _)    = pure $ [ ToHere (Change {c = b} Nothing) ]
inputs (All left rght)  = pure $ map ToLeft !(inputs left) ++ map ToRight !(inputs rght)
inputs (Any left rght)  = pure $ map ToLeft !(inputs left) ++ map ToRight !(inputs rght)
inputs this@(One _ _)   = pure $ map (ToHere . PickWith) (labels this) ++ map (ToHere . Pick) (choices this)
inputs (Fail)           = pure $ []
inputs (Then this _)    = inputs this
inputs (Next this next) = do
    always <- inputs this
    Just v <- value this | Nothing => pure always
    pure $ map ToHere (go (next v)) ++ always
  where
    go : TaskT m a -> List Action
    go task with (delabel task)
      | One _ _ = map ContinueWith $ labels task
      | Fail    = []
      | _       = [ Continue ]
inputs (Label _ this)   = inputs this
inputs (Lift _)         = pure $ []



-- Predicates ------------------------------------------------------------------


failing : TaskT m a -> Bool
failing (Edit _)        = False
failing (Store _)       = False
failing (All left rght) = failing left && failing rght
failing (Any left rght) = failing left && failing rght
failing (One left rght) = failing left && failing rght
failing (Fail)          = True
failing (Then this _)   = failing this
failing (Next this _)   = failing this
failing (Label _ this)  = failing this
failing (Lift _)        = False



-- Normalisation ---------------------------------------------------------------


normalise : MonadRef l m => TaskT m a -> m (TaskT m a)

-- Step --
normalise (Then this cont) = do
  this_new <- normalise this
  case !(value this_new) of
    Nothing => pure $ Then this_new cont
    Just v  =>
      --FIXME: should we use normalise here instead of just eval?
      let next = cont v in
      if failing next then
        pure $ Then this_new cont
      else
        normalise next

-- Evaluate --
normalise (All left rght) = do
  left_new <- normalise left
  rght_new <- normalise rght
  pure $ All left_new rght_new

normalise (Any left rght) = do
  left_new <- normalise left
  case !(value left_new) of
    Just _  => pure $ left_new
    Nothing => do
      rght_new <- normalise rght
      case !(value rght_new) of
        Just _  => pure $ rght_new
        Nothing => pure $ Any left_new rght_new

normalise (Next this cont) = do
  this_new <- normalise this
  pure $ Next this_new cont

-- Label --
normalise (Label l this) with (keeper this)
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


initialise : MonadRef l m => TaskT m a -> m (TaskT m a)
initialise = normalise



-- Event handling --------------------------------------------------------------


covering
handle : MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Event -> m (TaskT m a)

-- Edit --
handle (Edit _) (ToHere Empty) =
  pure $ Edit Nothing

handle (Edit {a} val) (ToHere (Change {c} val_new)) with (decEq c a)
  handle (Edit _)   (ToHere (Change val_new))       | Yes Refl = pure $ Edit val_new
  handle (Edit val) (ToHere (Change _))             | No _     = trace CouldNotChange $ Edit val

handle (Store {b} loc) (ToHere (Change {c} val_new)) with (decEq c b)
  handle (Store loc) (ToHere (Change (Just val_new)))| Yes Refl = do
    loc := val_new
    pure $ Store loc
  handle (Store loc) (ToHere (Change (Nothing)))     | Yes Refl = trace CouldNotChange $ Store loc
  handle (Store loc) (ToHere (Change _))             | No _     = trace CouldNotChange $ Store loc

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
handle task@(One left _) (ToHere (Pick (GoLeft p))) =
  -- Go left
  --FIXME: add failing for left?
  handle (assert_smaller task (delabel left)) (ToHere (Pick p))

handle task@(One _ rght) (ToHere (Pick (GoRight p))) =
  -- Go rght
  --FIXME: add failing for rght?
  handle (assert_smaller task (delabel rght)) (ToHere (Pick p))

handle task (ToHere (Pick GoHere)) =
  -- Go here
  pure $ task

handle task@(One _ _) (ToHere (PickWith l)) =
  case find l task of
    Nothing => trace (CouldNotFind l) task
    --XXX: needs `assert_smaller` for totallity, be aware of long type checks...
    -- Just p  => handle task (assert_smaller (ToHere (PickWith l)) (ToHere (Pick p)))
    Just p  => handle task (ToHere (Pick p))

handle task@(Next this cont) (ToHere Continue) =
  -- When pressed continue rewrite to an internal step
  pure $ Then this cont

handle task@(Next this cont) (ToHere (ContinueWith l)) =
  case !(value this) of
    Nothing => trace CouldNotContinue task
    Just v  =>
      let
        next = cont v
      in
      case find l next of
        Nothing => trace (CouldNotFind l) task
        Just p  => handle next (ToHere (Pick p))

-- Pass to this --
handle (Then this cont) event = do
  -- Pass the event to this
  this_new <- handle this event
  pure $ Then this_new cont

handle (Next this cont) event = do
  -- Pass the event to this
  this_new <- handle this event
  pure $ Next this_new cont

-- Label --
handle (Label l this) event with (keeper this)
  | False = handle this event
  | True = do
      this_new <- handle this event
      pure $ Label l this_new

-- Rest --
handle task event =
  -- Case `Fail`: Evaluation continues indefinitely
  trace (CouldNotHandle event) task


covering
drive : MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Event -> m (TaskT m a)
drive task event =
  handle task event >>= normalise
