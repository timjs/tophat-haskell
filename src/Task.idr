module Task

import Control.Monad.State

import Task.Universe
import Task.Event
import Helpers

%default total
%access export

%hide Language.Reflection.Elab.Tactics.normalise

infix  6 =~, /~

infixr 4 #
infixl 3 <*>, <&>
infixr 2 <|>, <?>
infixl 1 >>=, >>?


-- Types -----------------------------------------------------------------------

-- Tasks --

data Task : (Type -> Type) -> Universe.Ty -> Type where
  -- Core
  Edit  : (val : Maybe (typeOf a)) -> Task m a
  Watch : MonadState (typeOf s) m => Task m s
  -- Parallel
  All   : Show (typeOf a) => Show (typeOf b) => (left : Task m a) -> (right : Task m b) -> Task m (PairTy a b)
  -- Choice
  Any   : Show (typeOf a) => (left : Task m a) -> (right : Task m a) -> Task m a
  One   : Show (typeOf a) => (left : Task m a) -> (right : Task m a) -> Task m a
  Fail  : Task m a
  -- Sequence
  Then  : Show (typeOf a) => (this : Task m a) -> (next : typeOf a -> Task m b) -> Task m b
  Next  : Show (typeOf a) => (this : Task m a) -> (next : typeOf a -> Task m b) -> Task m b
  -- Labels
  Label : Show (typeOf a) => Label -> (this : Task m a) -> Task m a


-- Errors --

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


-- Interfaces ------------------------------------------------------------------

-- Monoidal --

pure : (typeOf a) -> Task m a
pure = Edit . Just

(<&>) : Show (typeOf a) => Show (typeOf b) => Task m a -> Task m b -> Task m (PairTy a b)
(<&>) = All

unit : Task m (BasicTy UnitTy)
unit = pure ()

-- (<*>) : Show (typeOf a) => Show (typeOf b) => Task m (FunTy a b) -> Task m a -> Task m b
-- (<*>) t1 t2 = (\f,x => f x) <$> t1 <&> t2


-- Alternative --

(<|>) : Show (typeOf a) => Task m a -> Task m a -> Task m a
(<|>) = Any

(<?>) : Show (typeOf a) => Task m a -> Task m a -> Task m a
(<?>) = One

fail : Task m a
fail = Fail


-- Monad --

(>>=) : Show (typeOf a) => Task m a -> (typeOf a -> Task m b) -> Task m b
(>>=) = Then

(>>?) : Show (typeOf a) => Task m a -> (typeOf a -> Task m b) -> Task m b
(>>?) = Next

-- infixl 1 >>*
-- (>>*) : Show (typeOf a) => Task m a -> List (typeOf a -> (Bool, Task m b)) -> Task m b
-- (>>*) t fs            = t >>- convert fs where
--   convert : List (Universe.typeOf a -> (Bool, Task m b)) -> Universe.typeOf a -> Task m b
--   convert [] x        = fail
--   convert (f :: fs) x =
--     let
--       ( guard, next ) = f x
--     in
--     (if guard then next else fail) <|> convert fs x


-- Functor --

(<$>) : Show (typeOf a) => (typeOf a -> typeOf b) -> Task m a -> Task m b
(<$>) f t = do
  x <- t
  pure $ f x


-- Labels --

||| Infix operator to label a task
(#) : Show (typeOf a) => Label -> Task m a -> Task m a
(#) = Label

||| Get the current label, if one
label : Task m a -> Maybe Label
label (Label l _) = Just l
label _           = Nothing

||| Remove as much labels as possible from a task.
|||
||| Usefull to deeply match task constructors while ignoring labels.
delabel : Task m a -> Task m a
delabel (Label _ t) = delabel t
delabel t           = t

||| Match a label to a task.
(=~) : Label -> Task m a -> Bool
(=~) k (Label l _) = l == l
(=~) _ _           = False

||| Negation of `(=~)`.
(/~) : Label -> Task m a -> Bool
(/~) l t = not (l =~ t)

||| Collect all labels in an external choice
labels : Task m a -> List Label
labels (Label _ Fail)   = []
labels (Label l this)   = l :: labels this
labels (One left right) = labels left ++ labels right
-- --FIXME: should we also check for labels on the lhs of a step (see also `find`)?
-- labels (Then this _)    = labels this
-- labels (Next this _)    = labels this
labels _                = []

||| Depth first search for a label on a task tree.
|||
||| Returns the path of the found task.
find : Label -> Task m a -> Maybe Path
find k (Label l this) with ( k == l )
  | True                = Just GoHere
  | False               = find k this
find k (One left right) = map GoLeft (find k left) <|> map GoRight (find k right)
-- --FIXME: should we can send pick-events through to the lhs of a step (see also `labels`)?
-- find k (Then this _)    = find k this
-- find k (Next this _)    = find k this
find k _                = Nothing

||| Check if a task constructor keeps its label after stepping or loses it.
keeper : Task m a -> Bool
keeper (Edit _)  = True
keeper (All _ _) = True
keeper (Fail)    = True
keeper _         = False


-- Extras --

ask : (b : Universe.Basic.Ty) -> Task m (BasicTy b)
ask _ = Edit Nothing

watch : MonadState (typeOf s) m => Task m s
watch = Watch


-- Showing ---------------------------------------------------------------------

{-
ui : Show (typeOf a) => Task m a -> State -> String
ui (Edit (Just x))  _ = "□(" ++ show x ++ ")"
ui (Edit Nothing)   _ = "□(_)"
ui (Watch)          s = "■(" ++ show s ++ ")"
ui (All left right) s = ui left s ++ "   ⋈   " ++ ui right s
ui (Any left right) s = ui left s ++ "   ◆   " ++ ui right s
ui (One left right) s with ( delabel left, delabel right )
  | ( One _ _, One _ _ ) =                  ui left s ++ " ◇ " ++ ui right s
  | ( One _ _, _       ) =                  ui left s ++ " ◇ " ++ fromMaybe "…" (label right)
  | ( _,       One _ _ ) = fromMaybe "…" (label left) ++ " ◇ " ++ ui right s
  | ( _,       _       ) = fromMaybe "…" (label left) ++ " ◇ " ++ fromMaybe "…" (label right)
ui (Fail)           _ = "↯"
ui (Then this cont) s = ui this s ++ " ▶…"
ui (Next this cont) s = ui this s ++ " ▷…"
ui (Label l this)   s = l ++ " # " ++ ui this s
ui (Get)            _ = "↓"
ui (Put x)          _ = "↑(" ++ show x ++ ")"


-- Semantics -------------------------------------------------------------------

value : Task m a -> State -> Maybe (typeOf a)
value (Edit val)       _ = val
value (Watch)          s = Just s
value (All left right) s = MkPair <$> value left s <*> (value right s)
value (Any left right) s = value left s <|> value right s
value (Label _ this)   s = value this s
value (Get)            s = Just s
value (Put _)          _ = Just ()
-- The rest never has a value because:
--   * `One` and `Next` need to wait for an user choice
--   * `Fail` runs forever and doesn't produce a value
--   * `Then` transforms values to another type
value _                _ = Nothing

choices : Task m a -> List Path
choices (One left right) =
  --XXX: No with-block possible?
  case ( delabel left, delabel right ) of
    ( Fail, Fail  ) => []
    ( left, Fail  ) => map GoLeft (GoHere :: choices left)
    ( Fail, right ) => map GoRight (GoHere :: choices right)
    ( left, right ) => map GoLeft (GoHere :: choices left) ++ map GoRight (GoHere :: choices right)
choices _           = []

events : Task m a -> State -> List Event
events (Edit {a} val)   _ = [ ToHere (Change (Universe.defaultOf a)), ToHere Clear ]
events (Watch)          _ = [ ToHere (Change (Universe.defaultOf StateTy)) ]
events (All left right) s = map ToLeft (events left s) ++ map ToRight (events right s)
events (Any left right) s = map ToLeft (events left s) ++ map ToRight (events right s)
events this@(One _ _)   s = map (ToHere . PickAt) (labels this) ++ map (ToHere . Pick) (choices this)
events (Fail)           _ = []
events (Then this next) s = events this s
events (Next this next) s =
  let
    here =
      case value this s of
        Just v  => go (next v)
        Nothing => []
  in
  map ToHere here ++ events this s
  where
    go : Task m a -> List Action
    go task with ( delabel task )
      | One _ _ = map (Continue . Just) $ labels task
      | Fail    = []
      | _       = [ Continue Nothing ]
events (Label _ this)   s = events this s
events (Get)            _ = []
events (Put x)          _ = []

normalise : Task m a -> State -> ( Task m a, State )
-- Step --
normalise (Then this cont) state =
  let
    ( this_new, state_new ) = normalise this state
  in
  case value this_new state_new of
    Nothing => ( Then this_new cont, state_new )
    Just v  =>
      --FIXME: should we use normalise here instead of just eval?
      case cont v of
        Fail => ( Then this_new cont, state_new )
        next => normalise next state_new
-- Evaluate --
normalise (All left right) state =
  let
    ( left_new, state_new )    = normalise left state
    ( right_new, state_newer ) = normalise right state_new
  in
  ( All left_new right_new, state_newer )
normalise (Any left right) state =
  let
    ( left_new, state_new )    = normalise left state
    ( right_new, state_newer ) = normalise right state_new
  in
  case value left_new state_newer of
    Just _  => ( left_new, state_newer )
    Nothing =>
      case value right_new state_newer of
        Just _  => ( right_new, state_newer )
        Nothing => ( Any left_new right_new, state_newer )
normalise (Next this cont) state =
  let
    ( this_new, state_new ) = normalise this state
  in
  ( Next this_new cont, state_new )
-- Label --
normalise (Label l this) state with ( keeper this )
  | False = normalise this state
  | True  =
      let
        ( this_new, state_new ) = normalise this state
      in
      ( Label l this_new, state_new )
-- State --
normalise (Get) state =
  ( pure state, state )
normalise (Put x) state =
  ( unit, x )
-- Values --
normalise task state =
  ( task, state )

--FIXME: fix totallity...
handle : Task m a -> Event -> State -> Either NotApplicable ( Task m a, State )
-- Edit --
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

drive : Task m a -> Event -> State -> Either NotApplicable ( Task m a, State )
drive task event state =
  uncurry normalise <$> handle task event state
-- do
--   ( task_new, state_new ) <- handle task event state
--   ok $ normalise task_new state_new

init : Task m a -> ( Task m a, State )
init = flip normalise []

-}
