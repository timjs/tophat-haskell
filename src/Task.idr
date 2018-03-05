module Task

import Task.Type
import Task.Event

%default total
%access export


-- Types -----------------------------------------------------------------------

-- State --

State : Type
State = Int

-- Tasks --

data Task : Ty -> Type where
    -- Primitive combinators
    Seq  : Show (typeOf a) => Task a -> (typeOf a -> Task b) -> Task b
    Par  : Show (typeOf a) => Show (typeOf b) => Task a -> Task b -> Task (PAIR a b)
    -- User interaction
    Edit : Maybe (typeOf a) -> Task a
    -- Share interaction
    Get  : Task INT
    Put  : typeOf INT -> Task UNIT
    -- Lifting
    Pure : typeOf a -> Task a


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

(Show (typeOf a)) => Show (Task a) where
    show (Seq left cont)          = show left ++ " => <cont>"
    show (Par left right)         = "(" ++ show left ++ " | " ++ show right ++ ")"
    show (Edit val)               = "edit " ++ show @{editor_value} val
    show Get                      = "get"
    show (Put x)                  = "put " ++ show x ++ ""
    show (Pure x)                 = show x


-- Semantics -------------------------------------------------------------------

value : Task a -> Maybe (typeOf a)
value (Pure x)         = Just x
value (Edit val)       = val
value (Par left right) = Just (!(value left), !(value right))
value _                = Nothing

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
    case value left of
        -- ...and we have a value: we get on with the continuation
        Just v  => eval (cont v) state
        -- ...without a value: we stay put and have to wait for a value to appear.
        Nothing => ( task, state )
handle task@(Seq left cont) event state =
    let
    ( newLeft, newState ) = handle left event state
    in
    ( Seq newLeft cont, newState )
handle task@(Par left right) (First event) state =
    -- We pass on the event to left
    let
    ( newLeft, newState )    = handle left event state
    in
    ( Par newLeft right, newState )
handle task@(Par left right) (Second event) state =
    -- We pass on the event to right
    let
    ( newRight, newState )    = handle right event state
    in
    ( Par left newRight, newState )
handle task@(Edit {a} _) (Change {b} newVal) state =
    case decEq b a of
        Yes prf =>
            ( Edit (coerce prf newVal), state )
        No _ =>
            ( task, state )
-- FIXME: Should pass more unhandled events down or not...
handle task _ state =
    ( task, state )
    -- Case Pure: evaluation terminated
    -- Cases Get and Put: this case can't happen, it is already evaluated by `eval`
    -- FIXME: express this in the type system...

init : Task a -> State -> ( Task a, State )
init = eval
