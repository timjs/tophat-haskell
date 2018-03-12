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

oneStep : Task (BasicTy IntTy)
oneStep = do
    x <- ask 0
    inc x

twoSteps : Task (BasicTy IntTy)
twoSteps = do
    x <- ask 0
    y <- ask 0
    add x y

parallel : Task (PairTy (BasicTy IntTy) (BasicTy StringTy))
parallel = edit (Just 1) |*| edit (Just "Hello")

parallelStep : Task (BasicTy StringTy)
parallelStep = do
    ( n, m ) <- parallel
    edit (Just (unwords $ replicate (cast n) m))

parallelWatch : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
parallelWatch = watch |*| watch

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

autoStep : Task (BasicTy IntTy)
autoStep =
    ask 2 >>* \x =>
    if x >= 10
        then pure x
    else fail

partial -- due to `mod` on `0`
checkModulo : Task (BasicTy StringTy)
checkModulo =
    ask 1 >>* \x =>
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
main = uncurry run $ init choice3 (state 0)
