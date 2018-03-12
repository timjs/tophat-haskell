module Main

import Task
import Task.Type
import Task.Event

%default total


-- Tests -----------------------------------------------------------------------

int : Task (Basic IntTy)
int = pure 42

str : Task (Basic StringTy)
str = pure "Hello"

ask : Int -> Task (Basic IntTy)
ask x = edit (Just x)

inc : Int -> Task (Basic IntTy)
inc x = edit (Just $ x + 1)

add : Int -> Int -> Task (Basic IntTy)
add x y = edit (Just $ x + y)

append : String -> String -> Task (Basic StringTy)
append x y = edit (Just $ x ++ y)

pureStep : Task (Basic IntTy)
pureStep = do
    x <- int
    inc x

oneStep : Task (Basic IntTy)
oneStep = do
    x <- ask 0
    inc x

twoSteps : Task (Basic IntTy)
twoSteps = do
    x <- ask 0
    y <- ask 0
    add x y

parallel : Task (PairTy (Basic IntTy) (Basic StringTy))
parallel = edit (Just 1) |*| edit (Just "Hello")

parallelStep : Task (Basic StringTy)
parallelStep = do
    ( n, m ) <- parallel
    edit (Just (unwords $ replicate (cast n) m))

parallelWatch : Task (PairTy (Basic IntTy) (Basic IntTy))
parallelWatch = watch |*| watch

update : Task UnitTy
update = do
    x <- get
    y <- ask x
    put y
    x <- get
    y <- ask x
    put y

control : Task (PairTy UnitTy (Basic IntTy))
control = update |*| watch

choice : Task (Basic IntTy)
choice = ask 1 |+| ask 2

choice3 : Task (Basic IntTy)
choice3 = choice |+| ask 3

choice1 : Task (Basic IntTy)
choice1 = ask 2 |+| fail


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
    event <- get
    let ( nextTask, nextState ) = handle task event state
    run nextTask nextState

main : IO ()
main = uncurry run $ init choice1 (state 0)
