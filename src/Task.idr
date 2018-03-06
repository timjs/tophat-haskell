module Task

import Task.Type
import Task.Event

%default total
%access export


-- Types -----------------------------------------------------------------------

-- State --

STATE : Ty
STATE = INT

State : Type
State = typeOf STATE


-- Tasks --

data Task : Ty -> Type where
    -- Primitive combinators
    Seq   : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
    Par   : Show (typeOf a) => Show (typeOf b) => Task a -> Task b -> Task (PAIR a b)
    -- User interaction
    Edit  : Maybe (typeOf a) -> Task a
    Watch : Task STATE
    -- Share interaction
    Get   : Task STATE
    Put   : typeOf STATE -> Task UNIT
    -- Lifting
    Pure  : typeOf a -> Task a


-- Public interface ------------------------------------------------------------

pure : (typeOf a) -> Task a
pure = Pure

(>>=) : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
(>>=) = Seq

infixl 3 <&>
(<&>) : Show (typeOf a) => Show (typeOf b) => Task a -> Task b -> Task (PAIR a b)
(<&>) = Par

edit : Maybe (typeOf a) -> Task a
edit = Edit

watch : Task INT
watch = Watch

get : Task INT
get = Get

put : typeOf INT -> Task UNIT
put = Put

unit : Task UNIT
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
ui (Seq left cont)  s = ui left s ++ " => <cont>"
ui (Par left right) s = "(" ++ ui left s ++ " | " ++ ui right s ++ ")"
ui (Edit val)       _ = "edit " ++ show @{editor_value} val
ui Watch            s = "watch " ++ show s
ui Get              _ = "get"
ui (Put x)          _ = "put " ++ show x ++ ""
ui (Pure x)         _ = show x


-- Semantics -------------------------------------------------------------------

value : Task a -> State -> Maybe (typeOf a)
value (Pure x)         _ = Just x
value (Edit val)       _ = val
value Watch            s = Just s
value (Par left right) s = Just (!(value left s), !(value right s))
value _                _ = Nothing

eval : Task a -> State -> ( Task a, State )
-- Combinators
eval (Seq left cont) state =
    let
    ( newLeft, newState ) = eval left state
    in
    case newLeft of
        --FIXME: maybe add a eval here
        Pure a => ( cont a, newState )
        _      => ( Seq newLeft cont, newState )
eval (Par left right) state =
    let
    ( newLeft, newState )    = eval left state
    ( newRight, newerState ) = eval right newState
    in
    case ( newLeft, newRight ) of
        ( Pure a, Pure b )    => ( Pure ( a, b ), newerState )
        ( newLeft, newRight ) => ( Par newLeft newRight, newerState )
-- State
eval (Get) state =
    ( Pure state, state )
eval (Put x) state =
    ( unit, x )
-- Values
eval task state =
    ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
handle task@(Seq left cont) Continue state =
    -- If we pressed Continue...
    case value left state of
        -- ...and we have a value: we get on with the continuation
        Just v  => eval (cont v) state
        -- ...without a value: we stay put and have to wait for a value to appear.
        Nothing => ( task, state )
handle (Seq left cont) event state =
    let
    ( newLeft, newState ) = handle left event state
    in
    ( Seq newLeft cont, newState )
handle (Par left right) (First event) state =
    -- We pass on the event to left
    let
    ( newLeft, newState ) = handle left event state
    in
    ( Par newLeft right, newState )
handle (Par left right) (Second event) state =
    -- We pass on the event to right
    let
    ( newRight, newState ) = handle right event state
    in
    ( Par left newRight, newState )
handle (Edit _) Clear state =
    ( Edit Nothing, state )
handle (Edit {a} val) (Change {b} newVal) state with (decEq b a)
  handle (Edit _) (Change newVal) state | Yes Refl = ( Edit (Just newVal), state )
  handle (Edit val) _ state             | No _     = ( Edit val, state )
handle Watch (Change {b} newVal) state with (decEq b STATE)
  handle Watch (Change newVal) _ | Yes Refl = ( Watch, newVal )
  handle Watch (Change _) state  | No _     = ( Watch, state )
-- FIXME: Should pass more unhandled events down or not...
handle task _ state =
    ( task, state )
    -- Case Pure: evaluation terminated
    -- Cases Get and Put: this case can't happen, it is already evaluated by `eval`
    -- FIXME: express this in the type system...

init : Task a -> State -> ( Task a, State )
init = eval
