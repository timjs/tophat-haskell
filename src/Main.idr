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


edit : Int -> Task (PRIM INT)
edit = pure


--FIXME: due to some namespacing problem...
del : Nat -> List a -> List a
del = Helpers.delete
rep : Nat -> a -> List a -> List a
rep = Helpers.replace



-- Basics --


fourtytwo : Task (PRIM INT)
fourtytwo = pure 42


hello : Task (PRIM STRING)
hello = pure "Hello"


inc : Int -> Task (PRIM INT)
inc x = pure (x + 1)


add : Int -> Int -> Task (PRIM INT)
add x y = pure (x + y)


append : String -> String -> Task (PRIM STRING)
append x y = pure (x ++ y)



-- Steps --


pureStep : Task (PRIM INT)
pureStep = do
  x <- fourtytwo
  inc x


pureStep' : Task (PRIM INT)
pureStep' = "Hello" # (
    fourtytwo >>? \x =>
    inc x
  )


pureStep'' : Task (PRIM INT)
pureStep'' =
  ("Hello" # fourtytwo) >>? \x =>
  inc x


oneStep : Task (PRIM INT)
oneStep = do
  x <- edit 0
  inc x


oneStep' : Task (PRIM INT)
oneStep' =
  edit 0 >>? \x =>
  inc x


twoSteps : Task (PRIM INT)
twoSteps = do
  x <- edit 1
  y <- edit 2
  add x y


twoSteps' : Task (PRIM INT)
twoSteps' =
  edit 1 >>? \x =>
  edit 2 >>? \y =>
  add x y



-- Parallel --


parallel : Task (PAIR (PRIM INT) (PRIM STRING))
parallel = "Give an integer" # ask (PRIM INT) <&> hello


parallelStep : Task (PRIM STRING)
parallelStep = do
  ( n, m ) <- parallel
  pure (unwords $ replicate (cast n) m)


parallelStep' : Task (PRIM STRING)
parallelStep' =
  parallel >>? \( n, m ) =>
  pure (unwords $ replicate (cast n) m)



-- Normalisation --
--
-- FIXME: should these automatically simplify?


pair : Task (PAIR (PRIM INT) (PRIM INT))
pair = pure 3 <&> pure 8


inner : Task (PRIM INT)
inner = do
  ( x, y ) <- pair
  add x y


inner' : Task (PRIM INT)
inner' =
  pair >>? \( x, y ) =>
  add x y



-- Shared Data --

partial
editList : Task (PAIR (PRIM UNIT) (LIST (PRIM INT)))
editList = do
  l <- ref (LIST (PRIM INT)) []
  start l <&> watch l

where

  delete : Loc (LIST (PRIM INT)) -> Nat -> Task (PRIM UNIT)
  delete l i =
    modify (LIST (PRIM INT)) l (del i)

  replace : Loc (LIST (PRIM INT)) -> Nat -> Task (PRIM UNIT)
  replace l i =
    "Give a new value" # ask (PRIM INT) >>? \x =>
    modify (LIST (PRIM INT)) l (rep i x)

  change : Loc (LIST (PRIM INT)) -> Task (PRIM UNIT)
  change l =
    --NOTE: `deref` should be before the external step,
    --      otherwise we'll end up an a state where we show an editor with the list when the user entered an improper index.
    --      Compare this with iTasks though:
    --      `deref` should be before the external step because you cannot specify the `deref` inside the step list!
    deref (LIST (PRIM INT)) l >>= \xs =>
    "Give an index" # ask (PRIM INT) >>? \n =>
    let i = the Nat (cast n) in
    if i < List.length xs then
      "Delete" # delete l i <?> "Replace" # replace l i
    else
      fail

  prepend : Loc (LIST (PRIM INT)) -> Task (PRIM UNIT)
  prepend l =
    "Give a new value to prepend" # ask (PRIM INT) >>? \x =>
    modify (LIST (PRIM INT)) l ((::) x)

  clear : Loc (LIST (PRIM INT)) -> Task (PRIM UNIT)
  clear l =
    modify (LIST (PRIM INT)) l (const [])

  quit : Task (PRIM UNIT)
  quit = pure ()

  mutual
    partial
    repeat : Loc (LIST (PRIM INT)) -> Task (PRIM UNIT)
    repeat l = do
      "Prepend" # prepend l <?> "Clear" # clear l <?> "Change" # change l
      start l

    partial
    start : Loc (LIST (PRIM INT)) -> Task (PRIM UNIT)
    start l =
      "Edit" # repeat l <?> "Quit" # quit


update1 : Loc (PRIM INT) -> Task (PRIM INT)
update1 l = do
  n <- ask (PRIM INT)
  assign (PRIM INT) l n
  m <- ask (PRIM INT)
  modify (PRIM INT) l ((+) m)
  edit !(deref (PRIM INT) l)


update2 : Loc (PRIM INT) -> Task (PRIM UNIT)
update2 l =
  deref (PRIM INT) l >>= \x =>
  edit (x + 1) >>? \y =>
  assign (PRIM INT) l y >>= \() =>
  deref (PRIM INT) l >>= \u =>
  edit (u + 2) >>? \v =>
  assign (PRIM INT) l v


inspect : Show (typeOf a) => (Loc (PRIM INT) -> Task a) -> Task (PAIR a (PRIM INT))
inspect f = do
  l <- ref (PRIM INT) 0
  f l <&> watch l

-- inspect : Show (typeOf a) => Show (typeOf b) => (Loc b -> Task a) -> Task (PAIR a (PRIM b))
-- inspect {b} f = do
--   l <- init b
--   f l <&> watch l



-- Choices --


pick1 : Task (PRIM INT)
pick1 = fail <|> edit 0


pick2 : Task (PRIM INT)
pick2 = edit 1 <|> edit 2


pick3 : Task (PRIM INT)
pick3 = pick2 <|> edit 3


pick1' : Task (PRIM INT)
pick1' = "Fail" # fail <?> "Cont" # edit 0


pick2' : Task (PRIM INT)
pick2' = "Pick one of two" # ("First" # edit 1 <?> "Second" # edit 2)


pick3' : Task (PRIM INT)
pick3' = "Pick one of three" # (pick2' <?> "Third" # edit 3)



-- Guards --


auto : Task (PRIM STRING)
auto = do
  x <- ask (PRIM INT)
  if x >= 10 then pure "large" else fail


actions : Task (PRIM INT)
actions =
  ask (PRIM INT) >>? \x =>
  pick3


actions' : Task (PRIM INT)
actions' =
  ask (PRIM INT) >>? \x =>
  pick3'


guards : Task (PRIM STRING)
guards = do
  x <- ask (PRIM INT)
  "Large" # (if x >= 10 then pure "large" else fail)
    <?>
    "VeryLarge" # (if x >= 100 then pure "very large" else fail)


guards' : Task (PRIM STRING)
guards' = do
  ask (PRIM INT) >>? \x =>
  ("Large" # (if x >= 10 then pure "large" else fail)
    <?>
    "VeryLarge" # (if x >= 100 then pure "very large" else fail))


partial -- due to `mod` on `0`
branch : Task (PRIM STRING)
branch =
  edit 1 >>? \x =>
  if x `mod` 3 == 0 then
    pure "multiple of 3"
  else if x `mod` 5 == 0 then
    pure "multiple of 5"
  else
    fail



-- Empty edit --


empties : Task (PRIM INT)
empties = do
  ( x, y ) <- ask (PRIM INT) <&> ask (PRIM INT)
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
  putStrLn $ "Possibilities: " ++ show !(Task.inputs task)
  event <- get
  loop !(Task.run task event)


run : Show (typeOf a) => Task a -> IO ()
run task = loop !(Task.initialise task)


main : IO ()
main = run empties
