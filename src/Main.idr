module Main

import Task
import Task.Type
import Task.Event

%default total


-- Tests -----------------------------------------------------------------------

int : Task INT
int = Pure 42

str : Task STRING
str = Pure "Hello"

edit : Task INT
edit = Edit (Just 0)

add : Int -> Task INT
add x = Edit (Just $ x + 1)

append : String -> String -> Task STRING
append x y = Edit (Just $ x ++ y)

pureStep : Task INT
pureStep = Seq int add

oneStep : Task INT
oneStep = Seq edit add

parallel : Task (PAIR INT STRING)
parallel = Par (Edit (Just 1)) (Edit (Just "Hello"))

parallelStep : Task STRING
parallelStep = Seq parallel repeat
where
    repeat : ( Int, String ) -> Task STRING
    repeat ( n, m ) = Edit (Just (unwords $ replicate (cast n) m))


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

run : Show (valueOf a) => Task a -> State -> IO ()
run task state = do
    putStrLn $ show task
    event <- get
    let ( nextTask, nextState ) = handle task event state
    run nextTask nextState

main : IO ()
main = uncurry run $ init parallelStep 0
