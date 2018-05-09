module Main

import Task
import Task.Universe
import Task.Event

%default total


-- Tests -----------------------------------------------------------------------

int : Task (BasicTy IntTy)
int = Done 42

str : Task (BasicTy StringTy)
str = Done "Hello"

ask : Int -> Task (BasicTy IntTy)
ask x = Edit (Just x)

inc : Int -> Task (BasicTy IntTy)
inc x = Edit (Just $ x + 1)

add : Int -> Int -> Task (BasicTy IntTy)
add x y = Edit (Just $ x + y)

append : String -> String -> Task (BasicTy StringTy)
append x y = Edit (Just $ x ++ y)

pureStep : Task (BasicTy IntTy)
pureStep = do
    x <- int
    inc x

pureStep' : Task (BasicTy IntTy)
pureStep' =
    int >>? \x =>
    (inc x |+| Fail)

oneStep : Task (BasicTy IntTy)
oneStep = do
    x <- ask 0
    inc x

oneStep' : Task (BasicTy IntTy)
oneStep' =
    ask 0 >>? \x =>
    (inc x |+| Fail)

-- oneStep'' : Task (BasicTy IntTy)
-- oneStep'' =
--     ask 0 >>*
--         [ \x => ( True, inc x ) ]

twoSteps : Task (BasicTy IntTy)
twoSteps = do
    x <- ask 0
    y <- ask 0
    add x y

twoSteps' : Task (BasicTy IntTy)
twoSteps' =
    ask 0 >>? \x =>
    ((ask 0 >>? \y =>
    (add x y |+| Fail)) |+| Fail)

parallel : Task (PairTy (BasicTy IntTy) (BasicTy StringTy))
parallel = Edit Nothing |*| Edit (Just "Hello")

parallelStep : Task (BasicTy StringTy)
parallelStep = do
    ( n, m ) <- parallel
    Edit (Just (unwords $ replicate (cast n) m))

parallelStep' : Task (BasicTy StringTy)
parallelStep' =
    parallel >>? \( n, m ) =>
    (Edit (Just (unwords $ replicate (cast n) m)) |+| Fail)

parallelAuto : Task (BasicTy StringTy)
parallelAuto =
    parallel >>? \( n, m ) =>
    Edit (Just (unwords $ replicate (cast n) m))

parallelWatch : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
parallelWatch = Watch |*| Watch

stablise : Int -> Task (BasicTy IntTy)
stablise x = do
    y <- ask x
    Done y

pair : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
pair = Done 3 |*| Done 8

inner : Task (BasicTy IntTy)
inner = do
    ( x, y ) <- pair
    add x y

inner' : Task (BasicTy IntTy)
inner' =
    pair >>? \( x, y ) =>
    add x y

update : Task UnitTy
update = do
    x <- Get
    y <- ask x
    Put y
    x <- Get
    y <- ask x
    Put y

control : Task (PairTy UnitTy (BasicTy IntTy))
control = update |*| Watch

choice : Task (BasicTy IntTy)
choice = ask 1 |+| ask 2

choice3 : Task (BasicTy IntTy)
choice3 = choice |+| ask 3

choice1 : Task (BasicTy IntTy)
choice1 = ask 2 |+| Fail

auto : Task (BasicTy StringTy)
auto =
    ask 0 >>? \x =>
    if x >= 10 then Done "large" else Fail

actions : Task (BasicTy StringTy)
actions =
    ask 0 >>? \x =>
    (Done "first" |+| Done "second first" |+| Done "second second")

guarded : Task (BasicTy StringTy)
guarded =
    ask 0 >>? \x =>
    ((if x >= 10 then Done "large" else Fail) |+| (if x >= 100 then Done "very large" else Fail))

partial -- due to `mod` on `0`
checkModulo : Task (BasicTy StringTy)
checkModulo =
    ask 1 >>? \x =>
    if x `mod` 3 == 0 then
        Done "multiple of 3"
    else if x `mod` 5 == 0 then
        Done "multiple of 5"
    else
        Fail


askInt : Task (BasicTy IntTy)
askInt = Edit (Nothing)

test : Task (BasicTy IntTy)
test =
    askInt |*| askInt >>? \(x, y) =>
    pure (x + y)


-- Running ---------------------------------------------------------------------

%default covering

get : IO Event
get = do
    putStr "> "
    input <- getLine
    case Event.parse (words input) of
        Right event => do
            pure event
        Left msg => do
            putStrLn msg
            get

loop : Show (typeOf a) => Task a -> State -> IO ()
loop task state = do
    putStrLn $ ui task state
    putStrLn $ "Possibilities: " ++ show (options task state)
    event <- get
    let ( nextTask, nextState ) = handle task event state
    loop nextTask nextState

run : Show (typeOf a) => Task a -> IO ()
run t = uncurry loop $ init t (state 0)

main : IO ()
main = run test
