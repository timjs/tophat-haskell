module Main

import System

import Task
import Helpers


%default total



-- Tests -----------------------------------------------------------------------
--
-- NOTE: Tasks ending in `'` need user input
--

-- Helpers --


edit : Int -> Task (BASIC INT)
edit = pure


--FIXME: due to some namespacing problem...
del : Nat -> List a -> List a
del = Helpers.delete
rep : Nat -> a -> List a -> List a
rep = Helpers.replace



-- BASIC --


fourtytwo : Task (BASIC INT)
fourtytwo = pure 42


hello : Task (BASIC STRING)
hello = pure "Hello"


inc : Int -> Task (BASIC INT)
inc x = pure (x + 1)


add : Int -> Int -> Task (BASIC INT)
add x y = pure (x + y)


append : String -> String -> Task (BASIC STRING)
append x y = pure (x ++ y)



-- Steps --


pureStep : Task (BASIC INT)
pureStep = do
  x <- fourtytwo
  inc x


pureStep' : Task (BASIC INT)
pureStep' =
  fourtytwo >>? \x =>
  inc x


oneStep : Task (BASIC INT)
oneStep = do
  x <- edit 0
  inc x


oneStep' : Task (BASIC INT)
oneStep' =
  edit 0 >>? \x =>
  inc x


twoSteps : Task (BASIC INT)
twoSteps = do
  x <- edit 1
  y <- edit 2
  add x y


twoSteps' : Task (BASIC INT)
twoSteps' =
  edit 1 >>? \x =>
  edit 2 >>? \y =>
  add x y



-- Parallel --


parallel : Task (PAIR (BASIC INT) (BASIC STRING))
parallel = "Give an integer" # ask INT <&> hello


parallelStep : Task (BASIC STRING)
parallelStep = do
  ( n, m ) <- parallel
  pure (unwords $ replicate (cast n) m)


parallelStep' : Task (BASIC STRING)
parallelStep' =
  parallel >>? \( n, m ) =>
  pure (unwords $ replicate (cast n) m)



-- Normalisation --
--
-- FIXME: should these automatically simplify?


pair : Task (PAIR (BASIC INT) (BASIC INT))
pair = pure 3 <&> pure 8


inner : Task (BASIC INT)
inner = do
  ( x, y ) <- pair
  add x y


inner' : Task (BASIC INT)
inner' =
  pair >>? \( x, y ) =>
  add x y



{- Shared Data --


partial
editShared : Task (BASIC UNIT)
editShared =
  "Edit" # repeat <?> "Quit" # quit
where
  delete : Nat -> Task (BASIC UNIT)
  delete i =
    modify (del i)
  replace : Nat -> Task (BASIC UNIT)
  replace i =
    "Give a new value" # ask INT >>? \x =>
    modify (rep i x)
  change : Task (BASIC UNIT)
  change =
    "Give an index" # ask INT >>? \n =>
    let i = the Nat (cast n) in
    --FIXME: get should be evaluated underneath, and not be a primitive in the Task monad
    get >>= \xs =>
    if i <= List.length xs then
      "Delete" # delete i <?> "Replace" # replace i
    else
      fail
  prepend : Task (BASIC UNIT)
  prepend =
    "Give a new value" # ask INT >>? \x =>
    modify ((::) x)
  clear : Task (BASIC UNIT)
  clear =
    modify (const [])
  quit : Task (BASIC UNIT)
  quit = pure ()


  partial
  repeat : Task (BASIC UNIT)
  repeat = do
    "Prepend" # prepend <?> "Clear" # clear <?> "Change" # change
    editShared


-- update : Task (BASIC UNIT)
-- update =
--   get >>= \x =>
--   edit x >>? \y =>
--   put y


-- --FIXME: help!!!
-- update2 : Task (BASIC UNIT)
-- update2 = do
--   get >>= \x =>
--   edit (x+1) >>? \y =>
--   put y >>= \() =>
--   get >>= \u =>
--   edit (u+2) >>? \v =>
--   put v


inspect : Show (typeOf a) => Task a -> Task (PAIR a StateTy)
inspect t = t <&> watch


parallelWatch : Task (PAIR StateTy StateTy)
parallelWatch = inspect watch


-}



-- Choices --


pick1 : Task (BASIC INT)
pick1 = fail <|> edit 0


pick2 : Task (BASIC INT)
pick2 = edit 1 <|> edit 2


pick3 : Task (BASIC INT)
pick3 = pick2 <|> edit 3


pick1' : Task (BASIC INT)
pick1' = "Fail" # fail <?> "Cont" # edit 0


pick2' : Task (BASIC INT)
pick2' = "First" # edit 1 <?> "Second" # edit 2


pick3' : Task (BASIC INT)
pick3' = pick2' <?> "Third" # edit 3



-- Guards --


auto : Task (BASIC STRING)
auto = do
  x <- edit 0
  if x >= 10 then pure "large" else fail


actions : Task (BASIC INT)
actions =
  edit 0 >>? \x =>
  pick3


actions' : Task (BASIC INT)
actions' =
  edit 0 >>? \x =>
  pick3'


guards : Task (BASIC STRING)
guards =
  edit 0 >>? \x =>
  ("Large" # (if x >= 10 then pure "large" else fail) <?> "VeryLarge" # (if x >= 100 then pure "very large" else fail))


partial -- due to `mod` on `0`
branch : Task (BASIC STRING)
branch =
  edit 1 >>? \x =>
  if x `mod` 3 == 0 then
    pure "multiple of 3"
  else if x `mod` 5 == 0 then
    pure "multiple of 5"
  else
    fail



-- Empty edit --


empties : Task (BASIC INT)
empties = do
  ( x, y ) <- ask INT <&> ask INT
  pure (x + y)



-- Running ---------------------------------------------------------------------
%default covering


get : IO Event
get = do
  putStr ">> "
  input <- getLine
  case input of
    "quit" => System.exit 0
    _ =>
      case Event.parse (words input) of
        Right event => do
          pure event
        Left msg => do
          putStrLn msg
          get


loop : Show (typeOf a) => Task a -> IO ()
loop task = do
  putStrLn !(Task.ui task)
  putStrLn $ "Possibilities: " ++ show !(Task.events task)
  event <- get
  loop !(Task.run task event)


run : Show (typeOf a) => Task a -> IO ()
run task = loop !(Task.init task)


main : IO ()
main = run empties
