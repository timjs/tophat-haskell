module Task

import Task.Universe
import Task.Event

%default total
%access public export

%hide Language.Reflection.Elab.Tactics.normalise


-- Types -----------------------------------------------------------------------

-- State --

StateTy : Universe.Ty
StateTy = ListTy (BasicTy IntTy)

State : Type
State = typeOf StateTy


-- Tasks --

data Task : Universe.Ty -> Type where
  -- Primitive combinators
  Then  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
  Next  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
  All   : Show (typeOf a) => Show (typeOf b) => (left : Task a) -> (right : Task b) -> Task (PairTy a b)
  One   : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
  Any   : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
  -- User interaction
  Watch : Task StateTy
  Edit  : (val : Maybe (typeOf a)) -> Task a
  -- Labeling
  Label : Show (typeOf a) => Label -> (this : Task a) -> Task a
  -- Failure
  Fail  : Task a
  -- Share interaction
  Get   : Task StateTy
  Put   : (x : typeOf StateTy) -> Task (BasicTy UnitTy)

delabel : Task a -> Task a
delabel (Label _ t) = delabel t
delabel t           = t

infix 6 =~, /~
(=~) : Label -> Task a -> Bool
(=~) l' (Label l _) = l' == l
(=~) _ _            = False

(/~) : Label -> Task a -> Bool
(/~) l t = not (l =~ t)

keeper : Task a -> Bool
keeper (Edit _)  = True
keeper (All _ _) = True
keeper (Fail)    = True
keeper _         = False


-- Interface -------------------------------------------------------------------

pure : (typeOf a) -> Task a
pure = Edit . Just

unit : Task (BasicTy UnitTy)
unit = pure ()

(>>=) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>=) = Then

infixl 1 >>?
(>>?) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>?) = Next

infixr 3 <&>
(<&>) : Show (typeOf a) => Show (typeOf b) => Task a -> Task b -> Task (PairTy a b)
(<&>) = All

infixr 2 <?>
(<?>) : Show (typeOf a) => Task a -> Task a -> Task a
(<?>) = Any

infixr 2 <|>
(<|>) : Show (typeOf a) => Task a -> Task a -> Task a
(<|>) = One

infixr 4 #
(#) : Show (typeOf a) => Label -> Task a -> Task a
(#) = Label

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


-- Applicative and Functor --

-- (<*>) : Show (typeOf a) => Show (typeOf b) => Task (FUN a b) -> Task a -> Task b
-- (<*>) t1 t2 = do
--   f <- t1
--   x <- t2
--   pure $ f x

(<$>) : Show (typeOf a) => (typeOf a -> typeOf b) -> Task a -> Task b
(<$>) f t = do
  x <- t
  pure $ f x


-- Showing ---------------------------------------------------------------------

ui : Show (typeOf a) => Task a -> State -> String
ui (Fail)           _ = "↯"
ui (Then this cont) s = ui this s ++ " ▶…"
ui (Next this cont) s = ui this s ++ " ▷…"
ui (All left right) s = ui left s ++ "   ×   " ++ ui right s
--FIXME: should we present UI's to the user for every or branch?
ui (One left right) s = ui left s ++ "   ◇   " ++ ui right s
ui (Any left right) s = ui left s ++ "   ◆   " ++ ui right s
ui (Edit (Just x))  _ = "□(" ++ show x ++")"
ui (Edit Nothing)   _ = "□(_)"
ui (Watch)          s = "■(" ++ show s ++ ")"
ui (Label _ this)   s = ui this s
ui (Get)            _ = "↓"
ui (Put x)          _ = "↑(" ++ show x ++ ")"


-- Semantics -------------------------------------------------------------------

value : Task a -> State -> Maybe (typeOf a)
value (Edit val)       _ = val
value (Watch)          s = Just s
value (All left right) s = MkPair <$> value left s <*> (value right s)
--NOTE: Uses semigroup instance of Maybe here
value (Any left right) s = value left s <+> value right s
value (Label _ this)   s = value this s
value (Get)            s = Just s
value (Put _)          _ = Just ()
-- The rest never has a value because:
--   * `One` and `Next` need to wait for an user choice
--   * `Fail` runs forever and doesn't produce a value
--   * `Then` transforms values to another type
value _                _ = Nothing

mutual
  labels : Task a -> List Label
  labels (Label l task)   = l :: labels task
  labels (One left right) = choices left ++ choices right
  labels _                = []

  choices : Task a -> List Label
  choices task with ( delabel task )
    | Fail  = []
    | _     = labels task

events : Task a -> State -> List Event
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
    go task = case delabel task of
      One _ _ => map (Continue . Just) $ choices task
      Fail    => []
      _       => [ Continue Nothing ]
events (All left right) s = map ToLeft (events left s) ++ map ToRight (events right s)
events (Any left right) s = map ToLeft (events left s) ++ map ToRight (events right s)
events (One left right) s = map (ToHere . Pick) $ choices left ++ choices right
events (Edit {a} val)   _ = [ ToHere (Change (Universe.defaultOf a)), ToHere Clear ]
events (Watch)          _ = [ ToHere (Change (Universe.defaultOf StateTy)) ]
events (Fail)           _ = []
events (Label _ this)   s = events this s
events (Get)            _ = []
events (Put x)          _ = []

normalise : Task a -> State -> ( Task a, State )
-- Step
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
-- Evaluate
normalise (Next this cont) state =
  let
    ( this_new, state_new ) = normalise this state
  in
  ( Next this_new cont, state_new )
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
-- Labeling
normalise (Label l this) state =
  let
    ( this_new, state_new ) = normalise this state
  in
  ( Label l this_new, state_new )
-- State
normalise (Get) state =
  ( pure state, state )
normalise (Put x) state =
  ( unit, x )
-- Values
normalise task state =
  ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
handle task@(Next this cont) (ToHere (Continue mp)) state =
  -- If we pressed Continue...
  case value this state of
    -- ...and we have a value: we get on with the continuation,
    Just v =>
      case mp of
        --FIXME: prevent stepping to `Fail`???
        -- and automatically pick if we recieved a path
        Just label => handle (cont v) (ToHere (Pick label)) state
        -- or just continue otherwise
        Nothing   => normalise (cont v) state
    -- ...without a value: we stay put and have to wait for a value to appear.
    Nothing => ( task, state )
handle (Next this cont) event state =
  -- Pass the event to this
  let
    ( this_new, state_new ) = handle this event state
  in
  ( Next this_new cont, state_new )
handle (Then this cont) event state =
  -- Pass the event to this and normalise
  let
    ( this_new, state_new ) = handle this event state
  in
  normalise (Then this_new cont) state_new
--FIXME: normalise after each event handling of All and One???
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
handle (One left right) (ToHere (Pick label)) state =
  if label =~ left then
    normalise left state
  else if label =~ right then
    normalise right state
  else
    -- Stay
    ( One left right, state )
handle (Edit _) (ToHere Clear) state =
  ( Edit Nothing, state )
handle (Edit {a} val) (ToHere (Change {b} val_new)) state with (decEq b a)
  handle (Edit _) (ToHere (Change val_new)) state         | Yes Refl = ( Edit (Just val_new), state )
  handle (Edit val) _ state                               | No _     = ( Edit val, state )
handle Watch (ToHere (Change {b} val_new)) state with (decEq b StateTy)
  handle Watch (ToHere (Change val_new)) _       | Yes Refl = ( Watch, val_new )
  handle Watch _ state                           | No _     = ( Watch, state )
-- Labeling
handle (Label l this) event state with ( keeper this )
  | True =
      let
        ( this_new, state_new ) = handle this event state
      in
      ( Label l this_new, state_new )
  | False = handle this event state
handle (Fail) _ state =
  -- Evaluation continues indefinitely
  ( Fail, state )
handle (Get) _ state =
  -- This case can't happen, it is already evaluated by `normalise`
  ( Get, state )
handle (Put x) _ state =
  -- This case can't happen, it is already evaluated by `normalise`
  ( Put x, state )

init : Task a -> ( Task a, State )
init = flip normalise []
