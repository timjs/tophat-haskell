module Main

import Task
import Task.Universe
import Task.Event

%default total


-- Tests -----------------------------------------------------------------------

int : Task (BasicTy IntTy)
int = pure 42

str : Task (BasicTy StringTy)
str = pure "Hello"

ask : Int -> Task (BasicTy IntTy)
ask x = edit (Just x)

inc : Int -> Task (BasicTy IntTy)
inc x = edit (Just $ x + 1)

add : Int -> Int -> Task (BasicTy IntTy)
add x y = edit (Just $ x + y)

append : String -> String -> Task (BasicTy StringTy)
append x y = edit (Just $ x ++ y)

pureStep : Task (BasicTy IntTy)
pureStep = do
    x <- int
    inc x

pureStep' : Task (BasicTy IntTy)
pureStep' =
    int >>- \x =>
    (inc x |+| fail)

oneStep : Task (BasicTy IntTy)
oneStep = do
    x <- ask 0
    inc x

oneStep' : Task (BasicTy IntTy)
oneStep' =
    ask 0 >>- \x =>
    (inc x |+| fail)

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
    ask 0 >>- \x =>
    ((ask 0 >>- \y =>
    (add x y |+| fail)) |+| fail)

parallel : Task (PairTy (BasicTy IntTy) (BasicTy StringTy))
parallel = edit Nothing |*| edit (Just "Hello")

parallelStep : Task (BasicTy StringTy)
parallelStep = do
    ( n, m ) <- parallel
    edit (Just (unwords $ replicate (cast n) m))

parallelStep' : Task (BasicTy StringTy)
parallelStep' =
    parallel >>- \( n, m ) =>
    (edit (Just (unwords $ replicate (cast n) m)) |+| fail)

parallelAuto : Task (BasicTy StringTy)
parallelAuto =
    parallel >>- \( n, m ) =>
    edit (Just (unwords $ replicate (cast n) m))

parallelWatch : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
parallelWatch = watch |*| watch

stablise : Int -> Task (BasicTy IntTy)
stablise x = do
    y <- ask x
    pure y

pair : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
pair = pure 3 |*| pure 8

inner : Task (BasicTy IntTy)
inner = do
    ( x, y ) <- pair
    add x y

inner' : Task (BasicTy IntTy)
inner' =
    pair >>- \( x, y ) =>
    add x y

update : Task UnitTy
update = do
    x <- get
    y <- ask x
    put y
    x <- get
    y <- ask x
    put y

control : Task (PairTy UnitTy (BasicTy IntTy))
control = update |*| watch

choice : Task (BasicTy IntTy)
choice = ask 1 |+| ask 2

choice3 : Task (BasicTy IntTy)
choice3 = choice |+| ask 3

choice1 : Task (BasicTy IntTy)
choice1 = ask 2 |+| fail

auto : Task (BasicTy StringTy)
auto =
    ask 0 >>- \x =>
    if x >= 10 then pure "large" else fail

actions : Task (BasicTy StringTy)
actions =
    ask 0 >>- \x =>
    (pure "first" |+| pure "second first" |+| pure "second second")

guarded : Task (BasicTy StringTy)
guarded =
    ask 0 >>- \x =>
    ((if x >= 10 then pure "large" else fail) |+| (if x >= 100 then pure "very large" else fail))

partial -- due to `mod` on `0`
checkModulo : Task (BasicTy StringTy)
checkModulo =
    ask 1 >>- \x =>
    if x `mod` 3 == 0 then
        pure "multiple of 3"
    else if x `mod` 5 == 0 then
        pure "multiple of 5"
    else
        fail


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

run : Show (typeOf a) => Task a -> State -> IO ()
run task state = do
    putStrLn $ ui task state
    putStrLn $ "Possibilities: " ++ show (options task state)
    event <- get
    let ( nextTask, nextState ) = handle task event state
    run nextTask nextState

main : IO ()
main = uncurry run $ init parallelStep (state 0)
