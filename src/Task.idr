module Task

%default total


-- Universe --------------------------------------------------------------------

data TaskType
    = UNIT
    | INT
    | STRING
    | PAIR TaskType TaskType

valueOf : TaskType -> Type
valueOf UNIT       = ()
valueOf INT        = Int
valueOf STRING     = String
valueOf (PAIR a b) = ( valueOf a, valueOf b )

Uninhabited (UNIT = INT) where
    uninhabited Refl impossible

Uninhabited (UNIT = STRING) where
    uninhabited Refl impossible

Uninhabited (UNIT = PAIR x y) where
    uninhabited Refl impossible

Uninhabited (INT = STRING) where
    uninhabited Refl impossible

Uninhabited (INT = PAIR x y) where
    uninhabited Refl impossible

Uninhabited (STRING = PAIR x y) where
    uninhabited Refl impossible

snd_neq : (y = y' -> Void) -> (PAIR x y = PAIR x y') -> Void
snd_neq contra Refl = contra Refl

fst_neq : (x = x' -> Void) -> (PAIR x y = PAIR x' y) -> Void
fst_neq contra Refl = contra Refl

both_neq : (x = x' -> Void) -> (y = y' -> Void) -> (PAIR x y = PAIR x' y') -> Void
both_neq contra_x contra_y Refl = contra_x Refl

DecEq TaskType where
    decEq UNIT       UNIT                                             = Yes Refl
    decEq INT        INT                                              = Yes Refl
    decEq STRING     STRING                                           = Yes Refl
    decEq (PAIR x y) (PAIR x' y')     with (decEq x x')
      decEq (PAIR x y) (PAIR x y')    | (Yes Refl)  with (decEq y y')
        decEq (PAIR x y) (PAIR x y)   | (Yes Refl)  | (Yes Refl)      = Yes Refl
        decEq (PAIR x y) (PAIR x y')  | (Yes Refl)  | (No contra)     = No (snd_neq contra)
      decEq (PAIR x y) (PAIR x' y')   | (No contra) with (decEq y y')
        decEq (PAIR x y) (PAIR x' y)  | (No contra) | (Yes Refl)      = No (fst_neq contra)
        decEq (PAIR x y) (PAIR x' y') | (No contra) | (No contra')    = No (both_neq contra contra')
    decEq UNIT       INT                                              = No absurd
    decEq INT        UNIT                                             = No (negEqSym absurd)
    decEq UNIT       STRING                                           = No absurd
    decEq STRING     UNIT                                             = No (negEqSym absurd)
    decEq UNIT       (PAIR x y)                                       = No absurd
    decEq (PAIR x y) UNIT                                             = No (negEqSym absurd)
    decEq INT        STRING                                           = No absurd
    decEq STRING     INT                                              = No (negEqSym absurd)
    decEq INT        (PAIR x y)                                       = No absurd
    decEq (PAIR x y) INT                                              = No (negEqSym absurd)
    decEq STRING     (PAIR x y)                                       = No absurd
    decEq (PAIR x y) STRING                                           = No (negEqSym absurd)


-- Types -----------------------------------------------------------------------

State : Type
State = Int


-- Values --

data Value : TaskType -> Type where
    NoValue   : Value a
    JustValue : valueOf a -> Value a

coerce : (a = b) -> Value a -> Value b
coerce Refl x = x


-- Events --

data Event : Type where
    Change   : (a ** Value a) -> Event
    Continue : Event
    First    : Event -> Event
    Second   : Event -> Event


-- Tasks --

data Task : TaskType -> Type where
    -- Primitive combinators
    Seq  : Task a -> (valueOf a -> Task b) -> Task b
    Par  : Task a -> Task b -> Task (PAIR a b)
    -- User interaction
    Edit : Value a -> Task a
    -- Share interaction
    Get  : Task INT
    Put  : valueOf INT -> Task UNIT
    -- Lifting
    Pure : {a : TaskType} -> valueOf a -> Task a

unit : Task UNIT
unit = Pure ()


-- Showing ---------------------------------------------------------------------

Show (valueOf a) => Show (Value a) where
    show NoValue       = "<no value>"
    show (JustValue x) = show x

(Show (valueOf a)) => Show (Task a) where
    -- show (Seq {s} left cont)      = show @{s} left ++ " => <cont>"
    -- show (Par {s} {t} left right) = "(" ++ show @{s} left ++ " | " ++ show @{t} right ++ ")"
    show (Seq left cont)          = "... => <cont>"
    show (Par left right)         = "(... | ...)"
    show (Edit val)               = "edit " ++ show val
    show Get                      = "get"
    show (Put x)                  = "put " ++ show x ++ ""
    show (Pure x)                 = show x


-- Semantics -------------------------------------------------------------------

value : Task a -> Value a
value (Pure x)   = JustValue x
value (Edit val) = val
value _          = NoValue

normalise : Task a -> State -> ( Task a, State )
-- Combinators
normalise (Seq left cont) state =
    let
    ( newLeft, newState ) = normalise left state
    in
    case newLeft of
        --FIXME: maybe add a normalise here
        Pure a => ( cont a, newState )
        _      => ( Seq newLeft cont, newState )
normalise (Par left right) state =
    let
    ( newLeft, newState )    = normalise left state
    ( newRight, newerState ) = normalise right newState
    in
    case ( newLeft, newRight ) of
        ( Pure a, Pure b )    => ( Pure ( a, b ), newerState )
        ( newLeft, newRight ) => ( Par newLeft newRight, newerState )
-- State
normalise (Get) state =
    ( Pure state, state )
normalise (Put x) state =
    ( unit, x )
-- Values
normalise task state =
    ( task, state )

handle : Task a -> Event -> State -> ( Task a, State )
handle task@(Seq left cont) Continue state =
    -- If we pressed Continue...
    case value left of
        -- ...and we have a value: we get on with the continuation
        JustValue v => ( cont v, state )
        -- ...without a value: we stay put and have to wait for a value to appear.
        NoValue     => ( task, state )
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
handle task@(Edit {a} val) (Change (b ** newVal)) state =
    case decEq b a of
        Yes prf =>
            ( Edit (coerce prf newVal), state )
        No _ =>
            ( task, state )
-- FIXME: Should pass more unhandled events down or not...
handle task _ state =
    ( task, state )
    -- Case Pure: evaluation terminated
    -- Cases Get and Put: this case can't happen, it is already evaluated by `normalise`
    -- FIXME: express this in the type system...


-- Tests -----------------------------------------------------------------------

int : Task INT
int = Pure 42

str : Task STRING
str = Pure "Hello"

edit : Task INT
edit = Edit (JustValue 0)

add : Int -> Task INT
add x = Edit (JustValue $ x + 1)

append : String -> String -> Task STRING
append x y = Edit (JustValue $ x ++ y)

pureStep : Task INT
pureStep = Seq int add

oneStep : Task INT
oneStep = Seq edit add


-- Running ---------------------------------------------------------------------

usage : String
usage = unlines
    [ ":: Possible events are:"
    , "    change <val> : change current value to <val> "
    , "    clear        : clear current value"
    , "    cont         : continue with the next task"
    , "    fst <event>  : send <event> to the first task"
    , "    snd <event>  : send <event> to the second task"
    , "    help         : show this message"
    ]

parse : List String -> Either String Event
parse ["change", val] = Right $ Change (INT ** JustValue (cast val))
parse ["clear"]       = Right $ Change (INT ** NoValue)
parse ["cont"]        = Right $ Continue
parse ("fst" :: rest) = map First $ parse rest
parse ("snd" :: rest) = map Second $ parse rest
parse ["help"]        = Left usage
parse other           = Left $ "!! '" ++ unwords other ++ "' is not a valid command"

%default covering

get : IO Event
get = do
    putStr "tasks> "
    input <- getLine
    case parse (words input) of
        Right event => do
            pure event
        Left msg => do
            putStrLn msg
            get

run : Show (valueOf a) => Task a -> State -> IO ()
run task_ state = do
    let ( normalisedTask, newState ) = normalise task_ state
    putStrLn $ show normalisedTask
    event <- get
    let ( nextTask, nextState ) = handle normalisedTask event newState
    run nextTask nextState

main : IO ()
main = run oneStep 0
