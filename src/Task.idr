module Task

import Task.Universe
import Task.Event

%default total
%access public export


-- Types -----------------------------------------------------------------------

-- State --

StateTy : Universe.Ty
StateTy = BasicTy IntTy

State : Type
State = typeOf StateTy


-- Tasks --

data Task : Universe.Ty -> Type where
    -- Basic tasks
    Done  : (x : typeOf a) -> Task a
    Fail  : Task a
    Then  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
    -- Primitive combinators
    Next  : Show (typeOf a) => (this : Task a) -> (next : typeOf a -> Task b) -> Task b
    And   : Show (typeOf a) => Show (typeOf b) => (left : Task a) -> (right : Task b) -> Task (PairTy a b)
    Or    : Show (typeOf a) => (left : Task a) -> (right : Task a) -> Task a
    -- User interaction
    Edit  : (val : Maybe (typeOf a)) -> Task a
    Watch : Task StateTy
    -- Share interaction
    Get   : Task StateTy
    Put   : (x : typeOf StateTy) -> Task UnitTy


-- Interface -------------------------------------------------------------------

pure : (typeOf a) -> Task a
pure = Done

--TODO: swap Next and Then
(>>=) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>=) = Next

infixl 1 >>-
(>>-) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>-) = Then

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

unit : Task UnitTy
unit = Done ()

state : Int -> State
state = id

-- infixl 1 >>*
-- (>>*) : Show (typeOf a) => Task a -> List (typeOf a -> (Bool, Task b)) -> Task b
-- (>>*) t fs = t >>- convert fs where
--     convert : List (Universe.typeOf a -> (Bool, Task b)) -> Universe.typeOf a -> Task b
--     convert [] x        = fail
--     convert (f :: fs) x =
--         let
--         ( guard, next ) = f x
--         in
--         (if guard then next else fail) |+| convert fs x


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
ui (Done x)         _ = "done " ++ show x
ui Fail             _ = "fail"
ui (Then this cont) s = ui this s ++ " => <cont>"
ui (Next this cont) s = ui this s ++ " -> <cont>"
ui (And left right) s = "(" ++ ui left s ++ " * " ++ ui right s ++ ")"
ui (Or left right)  s = "(" ++ ui left s ++ " + " ++ ui right s ++ ")"
ui (Edit val)       _ = "edit " ++ show @{editor_value} val
ui Watch            s = "watch " ++ show s
ui Get              _ = "get"
ui (Put x)          _ = "put " ++ show x ++ ""


-- Predicates ------------------------------------------------------------------

-- FIXME: is this correct?
isStable : Task a -> Bool
isStable (Done x)         = True
isStable Fail             = False
isStable (Then this cont) = isStable this
isStable (Next this cont) = isStable this
isStable (And left right) = isStable left && isStable right
isStable (Or left right)  = isStable left && isStable right
isStable (Edit x)         = False
isStable Watch            = False
isStable Get              = True
isStable (Put x)          = True


-- Semantics -------------------------------------------------------------------

value : Task a -> State -> Maybe (typeOf a)
value (Done x)         _ = Just x
value (Edit val)       _ = val
value Watch            s = Just s
value (And left right) s = Just (!(value left s), !(value right s))
-- The rest never has a value because:
--   * `Or` needs to wait for an user choice
--   * `Fail` runs forever and doesn't produce a value
--   * `Next` transforms values to another type
value _                _ = Nothing

choices : Task a -> List Path
choices (Or Fail Fail)  = []
choices (Or left Fail)  = [ First ]
choices (Or Fail right) = [ Second ] ++ map Other (choices right)
choices (Or left right) = [ First, Second ] ++ map Other (choices right)
choices _               = []

options : Task a -> State -> List Event
options (Done x)             _ = []
options (Next this next)     s =
    let
    here =
        case value this s of
            Just _  => [ Here Continue ]
            Nothing => []
    in
    here ++ options this s
options (Then this next)     s = options this s
    -- let
    -- here =
    --     case value this s of
    --         Just v  => map (Here . Execute) (choices $ next v)
    --         Nothing => []
    -- in
    -- here ++ options this s
options (And left right)     s = map ToLeft (options left s) ++ map ToRight (options right s)
options task@(Or left right) s = map (Here . Pick) (choices task) ++ map ToLeft (options left s) ++ map ToRight (options right s)
options (Edit {a} val)       _ = [ Here (Change (Universe.defaultOf a)), Here Empty ]
options Watch                _ = [ Here (Change (Universe.defaultOf StateTy)) ]
options Fail                 _ = []
options Get                  _ = []
options (Put x)              _ = []

normalise : Task a -> State -> ( Task a, State )
-- Step
normalise task@(Then this cont) state =
    --FIXME: normalise before???
    case value this state of
        Just v =>
            case cont v of
                Fail   => ( task, state )
                next   => normalise next state
        Nothing =>
            ( task, state )
-- Evaluate
normalise (Next this cont) state =
    let
    ( newThis, newState ) = normalise this state
    in
    ( Next newThis cont, newState )
normalise (And left right) state =
    let
    ( newLeft, newState )    = normalise left state
    ( newRight, newerState ) = normalise right newState
    in
    ( And newLeft newRight, newerState )
normalise (Or left right) state =
    let
    ( newLeft, newState )    = normalise left state
    ( newRight, newerState ) = normalise right newState
    in
    ( Or newLeft newRight, newerState )
-- State
normalise (Get) state =
    ( Done state, state )
normalise (Put x) state =
    ( unit, x )
-- Values
normalise task state =
    ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
handle task@(Next this cont) (Here Continue) state =
    -- If we pressed Continue...
    case value this state of
        -- ...and we have a value: we get on with the continuation
        Just v  => normalise (cont v) state
        -- ...without a value: we stay put and have to wait for a value to appear.
        Nothing => ( task, state )
handle (Next this cont) event state =
    -- Pass the event to this
    let
    ( newThis, newState ) = handle this event state
    --FIXME: maybe add a normalise here
    -- ( newerThis, newerState ) = normalise newThis newState
    in
    ( Next newThis cont, newState )
-- handle task@(Then this cont) (Here (Execute p)) state =
--     case value this state of
--         Just v =>
--             case handle (cont v) (Here (Pick p)) state of
--                 ( Fail, _ )        => ( task, state )
--                 ( next, newState ) => ( next, newState )
--         Nothing =>
--             ( task, state )
handle (Then this cont) event state =
    -- Pass the event to this and normalise
    let
    ( newThis, newState ) = handle this event state
    in
    normalise (Then newThis cont) newState
--FIXME: normalise after each event handling of And and Or???
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
handle (Or left right) (Here (Pick First)) state =
    -- Pick the first
    ( left, state )
handle (Or left right) (Here (Pick Second)) state =
    -- Pick the second
    ( right, state )
handle (Or left right) (Here (Pick (Other p))) state =
    -- Pick the second and continue
    handle right (Here (Pick p)) state
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
handle (Edit _) (Here Empty) state =
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
    -- Case `Done`: evaluation terminated
    -- Case `Fail`: evaluation continues indefinitely
    -- Case `Then`: should already be evaluated by `normalise`, otherwise pass events to `this`
    -- Cases `Get` and `Put`: this case can't happen, it is already evaluated by `normalise`
    -- FIXME: express this in the type system...

init : Task a -> State -> ( Task a, State )
init = normalise
