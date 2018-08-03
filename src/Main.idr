module Main

import System

import Task
import Task.Universe
import Task.Event
import Helpers

%default total

-- Tests -----------------------------------------------------------------------
--
-- NOTE: Tasks ending in `'` need user input
--

-- Helpers --

edit : Int -> Task (BasicTy IntTy)
edit = pure

ask : Task (BasicTy IntTy)
ask = Edit Nothing

modify : (List Int -> List Int) -> Task (BasicTy UnitTy)
modify f = do
  xs <- get
  put (f xs)

gets : (List Int -> (typeOf b)) -> Task b
gets f = do
  xs <- get
  pure (f xs)


-- Basic --

fourtytwo : Task (BasicTy IntTy)
fourtytwo = pure 42

hello : Task (BasicTy StringTy)
hello = pure "Hello"

inc : Int -> Task (BasicTy IntTy)
inc x = pure (x + 1)

add : Int -> Int -> Task (BasicTy IntTy)
add x y = pure (x + y)

append : String -> String -> Task (BasicTy StringTy)
append x y = pure (x ++ y)


-- Steps --

pureStep : Task (BasicTy IntTy)
pureStep = do
  x <- fourtytwo
  inc x

pureStep' : Task (BasicTy IntTy)
pureStep' =
  fourtytwo >>? \x =>
  inc x

oneStep : Task (BasicTy IntTy)
oneStep = do
  x <- edit 0
  inc x

oneStep' : Task (BasicTy IntTy)
oneStep' =
  edit 0 >>? \x =>
  inc x

twoSteps : Task (BasicTy IntTy)
twoSteps = do
  x <- edit 1
  y <- edit 2
  add x y

twoSteps' : Task (BasicTy IntTy)
twoSteps' =
  edit 1 >>? \x =>
  edit 2 >>? \y =>
  add x y


-- Parallel --

parallel : Task (PairTy (BasicTy IntTy) (BasicTy StringTy))
parallel = Edit Nothing <&> Edit (Just "Hello")

parallelStep : Task (BasicTy StringTy)
parallelStep = do
  ( n, m ) <- parallel
  pure (unwords $ replicate (cast n) m)

parallelStep' : Task (BasicTy StringTy)
parallelStep' =
  parallel >>? \( n, m ) =>
  pure (unwords $ replicate (cast n) m)


-- Normalisation --
--
-- FIXME: should these automatically simplify?

pair : Task (PairTy (BasicTy IntTy) (BasicTy IntTy))
pair = pure 3 <&> pure 8

inner : Task (BasicTy IntTy)
inner = do
  ( x, y ) <- pair
  add x y

inner' : Task (BasicTy IntTy)
inner' =
  pair >>? \( x, y ) =>
  add x y


-- Shared Data --

del : Nat -> List a -> List a
del = Helpers.delete
rep : Nat -> a -> List a -> List a
rep = Helpers.replace

partial
editShared : Task (BasicTy UnitTy)
editShared =
  repeat <?> "Quit" # quit
where
  delete : Nat -> Task (BasicTy UnitTy)
  delete i =
    modify (del i)
  replace : Nat -> Task (BasicTy UnitTy)
  replace i =
    ask >>? \x =>
    modify (rep i x)
  change : Task (BasicTy UnitTy)
  change =
    ask >>? \n =>
    let i = the Nat (cast n) in
    get >>= \xs =>
    if i <= List.length xs then
      "Delete" # delete i <?> "Replace" # replace i
    else
      Fail
  prepend : Task (BasicTy UnitTy)
  prepend =
    ask >>? \x =>
    modify ((::) x)
  clear : Task (BasicTy UnitTy)
  clear =
    modify (const [])
  quit : Task (BasicTy UnitTy)
  quit = pure ()

  partial
  repeat : Task (BasicTy UnitTy)
  repeat = do
    "Prepend" # prepend <?> "Clear" # clear <?> "Change" # change
    editShared

-- update : Task (BasicTy UnitTy)
-- update =
--   get >>= \x =>
--   edit x >>? \y =>
--   put y

-- --FIXME: help!!!
-- update2 : Task (BasicTy UnitTy)
-- update2 = do
--   get >>= \x =>
--   edit (x+1) >>? \y =>
--   put y >>= \() =>
--   get >>= \u =>
--   edit (u+2) >>? \v =>
--   put v

watch : Show (typeOf a) => Task a -> Task (PairTy a StateTy)
watch t = t <&> Watch

parallelWatch : Task (PairTy StateTy StateTy)
parallelWatch = watch Watch


-- Choices --

pick1 : Task (BasicTy IntTy)
pick1 = Fail <|> edit 0

pick2 : Task (BasicTy IntTy)
pick2 = edit 1 <|> edit 2

pick3 : Task (BasicTy IntTy)
pick3 = pick2 <|> edit 3

pick1' : Task (BasicTy IntTy)
pick1' = "Fail" # Fail <?> "Cont" # edit 0

pick2' : Task (BasicTy IntTy)
pick2' = "First" # edit 1 <?> "Second" # edit 2

pick3' : Task (BasicTy IntTy)
pick3' = pick2' <?> "Third" # edit 3

auto : Task (BasicTy StringTy)
auto = do
  x <- edit 0
  if x >= 10 then pure "large" else Fail

events : Task (BasicTy IntTy)
events =
  edit 0 >>? \x =>
  pick3

events' : Task (BasicTy IntTy)
events' =
  edit 0 >>? \x =>
  pick3'

guards : Task (BasicTy StringTy)
guards =
  edit 0 >>? \x =>
  ((if x >= 10 then pure "large" else Fail) <|> (if x >= 100 then pure "very large" else Fail))

partial -- due to `mod` on `0`
branch : Task (BasicTy StringTy)
branch =
  edit 1 >>? \x =>
  if x `mod` 3 == 0 then
    pure "multiple of 3"
  else if x `mod` 5 == 0 then
    pure "multiple of 5"
  else
    Fail


-- Empty edit --

test : Task (BasicTy IntTy)
test = do
  ( x, y ) <- ask <&> ask
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

loop : Show (typeOf a) => Task a -> State -> IO ()
loop task state = do
  putStrLn $ ui task state
  putStrLn $ "Possibilities: " ++ show (events task state)
  event <- get
  case drive task event state of
    Left error => do
      putStrLn $ "!! " ++ (show error)
      loop task state
    Right ( task_new, state_new ) =>
      loop task_new state_new

run : Show (typeOf a) => Task a -> IO ()
run t = uncurry loop $ init t

main : IO ()
main = run test
