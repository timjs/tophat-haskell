module Task

import Task.Type
import Task.Event

%default total
%access export


-- Types -----------------------------------------------------------------------

-- State --

StateTy : Ty
StateTy = Basic IntTy

State : Type
State = typeOf StateTy


-- Tasks --

data Task : Ty -> Type where
    -- Pure values
    Pure  : (x : typeOf a) -> Task a
    -- Primitive combinators
    Seq   : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
    And   : Show (typeOf a) => Show (typeOf b) => (left : Task a) -> (right : Task b) -> Task (PairTy a b)
    Or    : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
    -- User interaction
    Edit  : (val : Maybe (typeOf a)) -> Task a
    Watch : Task StateTy
    -- Failing
    Fail  : Task a
    -- Share interaction
    Get   : Task StateTy
    Put   : (x : typeOf StateTy) -> Task UnitTy


-- Public interface ------------------------------------------------------------

pure : (typeOf a) -> Task a
pure = Pure

fail : Task a
fail = Fail

(>>=) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>=) = Seq

infixr 3 |*|
(|*|) : Show (typeOf a) => Show (typeOf b) => Task a -> Task b -> Task (PairTy a b)
(|*|) = And
-- FIXME: should do the same trick as below, but need to prove `((a, b), c) = (a, (b, c))` for PairTy
-- (|*|) (And x y) z = And x (And y z)
-- (|*|) x y         = And x y

infixr 2 |+|
(|+|) : Show (typeOf a) => Task a -> Task a -> Task a
(|+|) (Or x y) z = Or x (Or y z)
(|+|) x y        = Or x y

edit : Maybe (typeOf a) -> Task a
edit = Edit

watch : Task (Basic IntTy)
watch = Watch

get : Task (Basic IntTy)
get = Get

put : typeOf (Basic IntTy) -> Task UnitTy
put = Put

unit : Task UnitTy
unit = Pure ()

state : Int -> State
state = id


-- Applicative and Functor --

-- (<*>) : Show (typeOf a) => Show (typeOf b) => Task (FUN a b) -> Task a -> Task b
-- (<*>) t1 t2 = do
--     f <- t1
--     x <- t2
--     pure $ f x

(<$>) : Show (typeOf a) => (typeOf a -> typeOf b) -> Task a -> Task b
(<$>) f t = do
    x <- t
    pure $ f x


-- Showing ---------------------------------------------------------------------

[editor_value] Show a => Show (Maybe a) where
    show Nothing  = "<no value>"
    show (Just x) = show x

ui : Show (typeOf a) => Task a -> State -> String
ui (Pure x)         _ = "pure " ++ show x
ui Fail             _ = "fail"
ui (Seq this cont)  s = ui this s ++ " => <cont>"
ui (And left right) s = "(" ++ ui left s ++ " * " ++ ui right s ++ ")"
ui (Or left right)  s = "(" ++ ui left s ++ " + " ++ ui right s ++ ")"
ui (Edit val)       _ = "edit " ++ show @{editor_value} val
ui Watch            s = "watch " ++ show s
ui Get              _ = "get"
ui (Put x)          _ = "put " ++ show x ++ ""


-- Predicates ------------------------------------------------------------------

-- FIXME: is this correct?
isStable : Task a -> Bool
isStable (Pure x)         = True
isStable Fail             = False
isStable (Seq this cont)  = isStable this
isStable (And left right) = isStable left && isStable right
isStable (Or left right)  = isStable left && isStable right
isStable (Edit x)         = False
isStable Watch            = False
isStable Get              = True
isStable (Put x)          = True


-- Semantics -------------------------------------------------------------------

value : Task a -> State -> Maybe (typeOf a)
value (Pure x)         _ = Just x
value (Edit val)       _ = val
value Watch            s = Just s
value (And left right) s = Just (!(value left s), !(value right s))
-- The rest never has a value because:
--   * `Or` needs to wait for an user choice
--   * `Fail` runs forever and doesn't produce a value
--   * `Seq` transforms values to another type
value _                _ = Nothing

normalise : Task a -> State -> ( Task a, State )
-- Combinators
normalise (Seq this cont) state =
    let
    ( newThis, newState ) = normalise this state
    in
    case newThis of
        Pure a => normalise (cont a) newState
        _      => ( Seq newThis cont, newState )
normalise (And left right) state =
    let
    ( newLeft, newState )    = normalise left state
    ( newRight, newerState ) = normalise right newState
    in
    case ( newLeft, newRight ) of
        ( Pure a, Pure b )    => ( Pure ( a, b ), newerState )
        ( newLeft, newRight ) => ( And newLeft newRight, newerState )
normalise (Or left right) state =
    let
    ( newLeft, newState )    = normalise left state
    ( newRight, newerState ) = normalise right newState
    in
    ( Or newLeft newRight, newerState )
-- State
normalise (Get) state =
    ( Pure state, state )
normalise (Put x) state =
    ( unit, x )
-- Values
normalise task state =
    ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
handle task@(Seq this cont) (Here Continue) state =
    -- If we pressed Continue...
    case value this state of
        -- ...and we have a value: we get on with the continuation
        Just v  => normalise (cont v) state
        -- ...without a value: we stay put and have to wait for a value to appear.
        Nothing => ( task, state )
handle (Seq this cont) event state =
    -- Pass the event to this
    let
    ( newThis, newState ) = handle this event state
    --FIXME: maybe add a normalise here
    -- ( newerThis, newerState ) = normalise newThis newState
    in
    ( Seq newThis cont, newState )
handle (And left right) (ToLeft event) state =
    -- Pass the event to left
    let
    ( newLeft, newState ) = handle left event state
    in
    ( And newLeft right, newState )
handle (And left right) (ToRight event) state =
    -- Pass the event to right
    let
    ( newRight, newState ) = handle right event state
    in
    ( And left newRight, newState )
handle (Or left right) (Here (Choose First)) state =
    -- Choose the first
    ( left, state )
handle (Or left right) (Here (Choose Second)) state =
    -- Choose the second
    ( right, state )
handle (Or left right) (Here (Choose (Next p))) state =
    -- Choose the second and continue
    handle right (Here (Choose p)) state
handle (Or left right) (ToLeft event) state =
    -- Pass the event to left
    let
    ( newLeft, newState ) = handle left event state
    in
    ( Or newLeft right, newState )
handle (Or left right) (ToRight event) state =
    -- Pass the event to right
    let
    ( newRight, newState ) = handle right event state
    in
    ( Or left newRight, newState )
handle (Edit _) (Here Clear) state =
    ( Edit Nothing, state )
handle (Edit {a} val) (Here (Change {b} newVal)) state with (decEq b a)
  handle (Edit _) (Here (Change newVal)) state | Yes Refl = ( Edit (Just newVal), state )
  handle (Edit val) _ state                    | No _     = ( Edit val, state )
handle Watch (Here (Change {b} newVal)) state with (decEq b StateTy)
  handle Watch (Here (Change newVal)) _ | Yes Refl = ( Watch, newVal )
  handle Watch _ state                  | No _     = ( Watch, state )
-- FIXME: Should pass more unhandled events down or not...
handle task _ state =
    ( task, state )
    -- Case `Pure`: evaluation terminated
    -- Case `Fail`: evaluation continues indefinitely
    -- Cases Get and Put: this case can't happen, it is already evaluated by `normalise`
    -- FIXME: express this in the type system...

init : Task a -> State -> ( Task a, State )
init = normalise
