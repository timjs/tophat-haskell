module Main

import Task
import Task.Type
import Task.Event

%default total


-- Tests -----------------------------------------------------------------------

int : Task INT
int = pure 42

str : Task STRING
str = pure "Hello"

ask : Int -> Task INT
ask x = edit (Just x)

add : Int -> Task INT
add x = edit (Just $ x + 1)

append : String -> String -> Task STRING
append x y = edit (Just $ x ++ y)

pureStep : Task INT
pureStep = do
    x <- int
    add x

oneStep : Task INT
oneStep = do
    x <- ask 0
    add x

parallel : Task (PAIR INT STRING)
parallel = edit (Just 1) <&> edit (Just "Hello")

parallelStep : Task STRING
parallelStep = do
    ( n, m ) <- parallel
    edit (Just (unwords $ replicate (cast n) m))

update : Task UNIT
update = do
    x <- get
    y <- ask x
    put y

watch : Task INT
watch = do
    x <- get
    edit (Just x)

control : Task (PAIR UNIT INT)
control = update <&> watch


-- Running ---------------------------------------------------------------------

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

run : Show (typeOf a) => Task a -> State -> IO ()
run task state = do
    putStrLn $ show task
    event <- get
    let ( nextTask, nextState ) = handle task event state
    run nextTask nextState

main : IO ()
main = uncurry run $ init control (state 0)
