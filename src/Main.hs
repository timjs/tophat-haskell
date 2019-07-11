module Main where

import Data.Task
import Data.Task.Run


main :: IO ()
main =
  run (add 3 4)


-- Examples --------------------------------------------------------------------
--
-- NOTE: Tasks ending with a quote need user input
--

-- Basics --

fourtytwo :: Task Int
fourtytwo =
  update 42

hello :: Task Text
hello =
  update "Hello"

inc :: Int -> Task Int
inc x =
  view (x + 1)

add :: Int -> Int -> Task Int
add x y =
  view (x + y)

append :: Text -> Text -> Task Text
append x y =
  view (x <> y)


-- Steps --

pureStep :: Task Int
pureStep = do
  x <- fourtytwo
  inc x

pureStep' :: Task Int
pureStep' = do
  x <- fourtytwo
  inc x <?> empty

oneStep :: Task Int
oneStep = do
  x <- update 0
  inc x

oneStep' :: Task Int
oneStep' = do
  x <- update 0
  inc x <?> empty

twoSteps :: Task Int
twoSteps = do
  x <- update 1
  y <- update 2
  add x y

twoSteps' :: Task Int
twoSteps' =
  update 1 >>? \x ->
  update 2 >>? \y ->
  add x y


-- Parallel --

parallel :: Task ( Int, Text )
parallel = enter <&> hello

parallelStep :: Task Text
parallelStep = do
  ( n, m ) <- parallel
  view (unwords <| replicate n m)

parallelStep' :: Task Text
parallelStep' = do
  ( n, m ) <- parallel
  view (unwords <| replicate n m) <?> empty


-- Choices --

choose1 :: Task Int
choose1 = empty <|> view 0

choose2 :: Task Int
choose2 = view 1 <|> view 2

choose3 :: Task Int
choose3 = choose2 <|> view 3

pick1 :: Task Int
pick1 = empty <?> view 0

pick2 :: Task Int
pick2 = view 1 <?> view 2

pick3 :: Task Int
pick3 = pick2 <?> view 3


-- Guards --

auto :: Task Text
auto = do
  x <- enter
  if x >= (10 :: Int)
    then view "large"
    else empty

actions :: Task Int
actions = do
  _ <- enter :: Task Int
  pick3

guards :: Task Text
guards = do
  x <- enter
  (if x >= (10 :: Int) then view "large" else empty)
    <?> (if x >= (100 :: Int) then view "very large" else empty)

branch :: Task Text
branch = do
  x <- update (1 :: Int)
  if x `mod` 3 == 0 then
    view "multiple of 3"
  else if x `mod` 5 == 0 then
    view "multiple of 5"
  else
    empty

branch' :: Task Text
branch' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else empty)
    <|> (if x `mod` 5 == 0 then view "multiple of 5" else empty)

branch'' :: Task Text
branch'' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else empty)
    <?> (if x `mod` 5 == 0 then view "multiple of 5" else empty)


{-
{- Shared Data --

partial
editList :: Task ( (), (LIST Int) )
editList = do
  l <- ref (LIST Int) []
  start l <&> watch l

  where

    delete :: Loc (LIST Int) -> Nat -> Task ()
    delete l i =
      modify (LIST Int) l (del i)

    replace :: Loc (LIST Int) -> Nat -> Task ()
    replace l i =
      "Give a new value" -#- enter >>? \x ->
      modify (LIST Int) l (rep i x)

    change :: Loc (LIST Int) -> Task ()
    change l =
      --NOTE: `deref` should be before the external step,
      --      otherwise we'll end up an a state where we show an editor with the list when the user entered an improper index.
      --      Compare this with iTasks though:
      --      `deref` should be before the external step because you cannot specify the `deref` inside the step list!
      deref (LIST Int) l >>= \xs ->
      "Give an index" -#- enter >>? \n ->
      let i = the Nat (cast n) in
      if i < List.length xs then
        "Delete" -#- delete l i <?> "Replace" -#- replace l i
      else
        empty

    prepend :: Loc (LIST Int) -> Task ()
    prepend l =
      "Give a new value to prepend" -#- enter >>? \x ->
      modify (LIST Int) l ((::) x)

    clear :: Loc (LIST Int) -> Task ()
    clear l =
      modify (LIST Int) l (const [])

    quit :: Task ()
    quit = update ()

    mutual
      partial
      repeat :: Loc (LIST Int) -> Task ()
      repeat l = do
        "Prepend" -#- prepend l <?> "Clear" -#- clear l <?> "Change" -#- change l
        start l

      partial
      start :: Loc (LIST Int) -> Task ()
      start l =
        "Edit" -#- repeat l <?> "Quit" -#- quit
-}

update1 :: Loc Int -> Task Int
update1 l = do
  n <- enter
  l <<- n
  m <- enter

  l <<- ((+) m)
  update !(deref l)

update2 :: Loc Int -> Task ()
update2 l =
  deref l >>= \x ->
  update (x + 1) >>? \y ->
  l $= const y >>= \() ->
  deref l >>= \u ->
  update (u + 2) >>? \v ->
  l $= const v


inspect :: Show (typeOf a) -> (Loc Int -> Task a) -> Task ( a, Int )
inspect f = do
  l <- Int 0
  f l <&> watch l

-- inspect :: Show (typeOf a) -> Show (typeOf b) -> (Loc b -> Task a) -> Task ( a, b )
-- inspect {b} f = do
--   l <- init b
--   f l <&> watch l


doubleShared :: Task ( (), Int )
doubleShared = do
  l <- ref 0
  m <- ref 0
  let t1 = do
    x <- watch m
    if x >= 10 then update (x * 2) else empty
  let t2 = do
    y <- watch l
    if y >= 5 then m $= const 12 else empty
  t2 <&> t1



-- Empty update --


empties :: Task Int
empties = do
  ( x, y ) <- enter <&> enter
  update (x + y)




-}
