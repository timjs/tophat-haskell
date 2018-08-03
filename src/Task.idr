module Task

import Task.Universe
import Task.Event

%default total
%access public export

%hide Language.Reflection.Elab.Tactics.normalise

infix  6 =~, /~

infixr 4 #
infixl 3 <*>, <&>
infixr 2 <|>, <?>
infixl 1 >>=, >>?


-- Types -----------------------------------------------------------------------

-- State --

StateTy : Universe.Ty
StateTy = ListTy (BasicTy IntTy)

State : Type
State = typeOf StateTy


-- Tasks --

data Task : Universe.Ty -> Type where
  -- Core
  Edit  : (val : Maybe (typeOf a)) -> Task a
  Watch : Task StateTy
  -- Parallel
  All   : Show (typeOf a) => Show (typeOf b) => (left : Task a) -> (right : Task b) -> Task (PairTy a b)
  -- Choice
  Any   : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
  One   : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
  Fail  : Task a
  -- Sequence
  Then  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
  Next  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
  -- Labels
  Label : Show (typeOf a) => Label -> (this : Task a) -> Task a
  -- State
  Get   : Task StateTy
  Put   : (x : typeOf StateTy) -> Task (BasicTy UnitTy)


-- Interfaces ------------------------------------------------------------------

-- Monoidal --

pure : (typeOf a) -> Task a
pure = Edit . Just

(<&>) : Show (typeOf a) => Show (typeOf b) => Task a -> Task b -> Task (PairTy a b)
(<&>) = All

unit : Task (BasicTy UnitTy)
unit = pure ()

-- (<*>) : Show (typeOf a) => Show (typeOf b) => Task (FunTy a b) -> Task a -> Task b
-- (<*>) t1 t2 = (\f,x => f x) <$> t1 <&> t2


-- Alternative --

(<|>) : Show (typeOf a) => Task a -> Task a -> Task a
(<|>) = Any

(<?>) : Show (typeOf a) => Task a -> Task a -> Task a
(<?>) = One

fail : Task a
fail = Fail


-- Monad --

(>>=) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>=) = Then

(>>?) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>?) = Next

-- infixl 1 >>*
-- (>>*) : Show (typeOf a) => Task a -> List (typeOf a -> (Bool, Task b)) -> Task b
-- (>>*) t fs            = t >>- convert fs where
--   convert : List (Universe.typeOf a -> (Bool, Task b)) -> Universe.typeOf a -> Task b
--   convert [] x        = fail
--   convert (f :: fs) x =
--     let
--       ( guard, next ) = f x
--     in
--     (if guard then next else fail) <|> convert fs x


-- Functor --

(<$>) : Show (typeOf a) => (typeOf a -> typeOf b) -> Task a -> Task b
(<$>) f t = do
  x <- t
  pure $ f x


-- Labels --

||| Infix operator to label a task
(#) : Show (typeOf a) => Label -> Task a -> Task a
(#) = Label

||| Remove as much labels as possible from a task.
|||
||| Usefull to deeply match task constructors while ignoring labels.
delabel : Task a -> Task a
delabel (Label _ t) = delabel t
delabel t           = t

||| Match a label to a task.
(=~) : Label -> Task a -> Bool
(=~) k (Label l _) = l == l
(=~) _ _           = False

||| Negation of `(=~)`.
(/~) : Label -> Task a -> Bool
(/~) l t = not (l =~ t)

||| Collect all labels in an external choice
labels : Task a -> List Label
labels (Label l this)   = l :: labels this
labels (One left right) = labels left ++ labels right
labels _                = []

||| Depth first search for a label on a task tree.
|||
||| Returns the path of the found task.
find : Label -> Task a -> Maybe Path
find k (Label l this) with ( k == l )
  | True                = Just GoHere
  | False               = find k this
find k (One left right) = map GoLeft (find k left) <|> map GoRight (find k right)
--FIXME: more cases needed?
find k _                = Nothing

||| Check if a task constructor keeps its label after stepping or loses it.
keeper : Task a -> Bool
keeper (Edit _)  = True
keeper (All _ _) = True
keeper (Fail)    = True
keeper _         = False


-- Showing ---------------------------------------------------------------------

ui : Show (typeOf a) => Task a -> State -> String
ui (Edit (Just x))  _ = "□(" ++ show x ++")"
ui (Edit Nothing)   _ = "□(_)"
ui (Watch)          s = "■(" ++ show s ++ ")"
ui (All left right) s = ui left s ++ "   ⋈   " ++ ui right s
ui (Any left right) s = ui left s ++ "   ◆   " ++ ui right s
ui (One left right) s =             "…   ◇   …"
ui (Fail)           _ = "↯"
ui (Then this cont) s = ui this s ++ " ▶…"
ui (Next this cont) s = ui this s ++ " ▷…"
ui (Label _ this)   s = ui this s
ui (Get)            _ = "↓"
ui (Put x)          _ = "↑(" ++ show x ++ ")"


-- Semantics -------------------------------------------------------------------

value : Task a -> State -> Maybe (typeOf a)
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

choices : Task a -> List Path
choices (One left right) =
  --XXX: No with-block possible?
  case ( delabel left, delabel right ) of
    ( Fail, Fail  ) => []
    ( left, Fail  ) => map GoLeft (GoHere :: choices left)
    ( Fail, right ) => map GoRight (GoHere :: choices right)
    ( left, right ) => map GoLeft (GoHere :: choices left) ++ map GoRight (GoHere :: choices right)
choices _           = []

events : Task a -> State -> List Event
events (Edit {a} val)   _ = [ ToHere (Change (Universe.defaultOf a)), ToHere Clear ]
events (Watch)          _ = [ ToHere (Change (Universe.defaultOf StateTy)) ]
events (All left right) s = map ToLeft (events left s) ++ map ToRight (events right s)
events (Any left right) s = map ToLeft (events left s) ++ map ToRight (events right s)
events this@(One _ _)   s = map (ToHere . Pick) $ map Left (labels this) ++ map Right (choices this)
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
    go : Task a -> List Action
    go task with ( delabel task )
      | One _ _ = map (Continue . Just) $ labels task
      | Fail    = []
      | _       = [ Continue Nothing ]
events (Label _ this)   s = events this s
events (Get)            _ = []
events (Put x)          _ = []

normalise : Task a -> State -> ( Task a, State )
-- Step --
normalise task@(Then this cont) state =
  --FIXME: normalise before???
  -- let
  --   ( this_new, state_new ) = normalise this state
  -- in
  case value this state of
    Just v  =>
      case cont v of
        Fail => ( task, state )
        next => normalise next state
    Nothing =>
      ( task, state )
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
  | True =
      let
        ( this_new, state_new ) = normalise this state
      in
      ( Label l this_new, state_new )
  | False = normalise this state
-- State --
normalise (Get) state =
  ( pure state, state )
normalise (Put x) state =
  ( unit, x )
-- Values --
normalise task state =
  ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
-- Edit --
handle (Edit _) (ToHere Clear) state =
  ( Edit Nothing, state )
handle (Edit {a} val) (ToHere (Change {b} val_new)) state with (decEq b a)
  handle (Edit _) (ToHere (Change val_new)) state         | Yes Refl = ( Edit (Just val_new), state )
  handle (Edit val) _ state                               | No _     = ( Edit val, state )
handle Watch (ToHere (Change {b} val_new)) state with (decEq b StateTy)
  handle Watch (ToHere (Change val_new)) _       | Yes Refl = ( Watch, val_new )
  handle Watch _ state                           | No _     = ( Watch, state )
-- Pass to this --
handle (Then this cont) event state =
  -- Pass the event to this and normalise
  let
    ( this_new, state_new ) = handle this event state
  in
  normalise (Then this_new cont) state_new
-- Pass to left or right --
--FIXME: normalise after each event handling of All, Any and One???
handle (All left right) (ToLeft event) state =
  -- Pass the event to left
  let
    ( left_new, state_new ) = handle left event state
  in
  ( All left_new right, state_new )
handle (All left right) (ToRight event) state =
  -- Pass the event to right
  let
    ( right_new, state_new ) = handle right event state
  in
  ( All left right_new, state_new )
handle (Any left right) (ToLeft event) state =
  -- Pass the event to left
  let
    ( left_new, state_new ) = handle left event state
  in
  ( Any left_new right, state_new )
handle (Any left right) (ToRight event) state =
  -- Pass the event to right
  let
    ( right_new, state_new ) = handle right event state
  in
  ( Any left right_new, state_new )
-- Interact
handle task@(One _ _) (ToHere (Pick (Left l))) state =
  case find l task of
    Just p  => handle task (ToHere (Pick (Right p))) state
    Nothing => ( task, state )
handle (One left _) (ToHere (Pick (Right (GoLeft p)))) state =
  -- Go left
  handle left (ToHere (Pick (Right p))) state
handle (One _ right) (ToHere (Pick (Right (GoRight p)))) state =
  -- Go right
  handle right (ToHere (Pick (Right p))) state
handle (One left right) (ToHere (Pick (Right GoHere))) state =
  -- Go here
  ( One left right, state )
handle task@(Next this cont) (ToHere (Continue Nothing)) state =
  -- When pressed continue rewrite to an internal step
  normalise (Then this cont) state
handle task@(Next this cont) (ToHere (Continue (Just l))) state =
  case value this state of
    Just v  => handle (cont v) (ToHere (Pick (Left l))) state
    Nothing => ( task, state )
handle (Next this cont) event state =
  -- Pass the event to this and normalise
  let
    ( this_new, state_new ) = handle this event state
  in
  normalise (Next this_new cont) state_new
-- Label
handle (Label l this) event state with ( keeper this )
  | True =
      let
        ( this_new, state_new ) = handle this event state
      in
      ( Label l this_new, state_new )
  | False = handle this event state
-- Rest
handle task _ state =
  -- Case `Fail`: Evaluation continues indefinitely
  -- Cases `Get` and `Put`: This case can't happen, it is already evaluated by `normalise`
  ( task, state )

init : Task a -> ( Task a, State )
init = flip normalise []
