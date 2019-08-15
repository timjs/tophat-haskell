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

fourtytwo :: Task m Int
fourtytwo =
  update 42

hello :: Task m Text
hello =
  update "Hello"

inc :: Int -> Task m Int
inc x =
  view (x + 1)

add :: Int -> Int -> Task m Int
add x y =
  view (x + y)

append :: Text -> Text -> Task m Text
append x y =
  view (x <> y)


-- Steps --

pureStep :: Task m Int
pureStep = do
  x <- fourtytwo
  inc x

pureStep' :: Task m Int
pureStep' = do
  x <- fourtytwo
  inc x <?> empty

oneStep :: Task m Int
oneStep = do
  x <- update 0
  inc x

oneStep' :: Task m Int
oneStep' = do
  x <- update 0
  inc x <?> empty

twoSteps :: Task m Int
twoSteps = do
  x <- update 1
  y <- update 2
  add x y

twoSteps' :: Task m Int
twoSteps' =
  update 1 >>? \x ->
  update 2 >>? \y ->
  add x y

twoSteps'' :: Task m Int
twoSteps'' = do
  x <- enter
  y <- enter
  add x y

-- Parallel --

parallel :: Task m ( Int, Text )
parallel = enter <&> hello

parallelStep :: Task m Text
parallelStep = do
  ( n, m ) <- parallel
  view (unwords <| replicate n m)

parallelStep' :: Task m Text
parallelStep' = do
  ( n, m ) <- parallel
  view (unwords <| replicate n m) <?> empty


-- Choices --

choose1 :: Task m Int
choose1 = empty <|> view 0

choose2 :: Task m Int
choose2 = view 1 <|> view 2

choose3 :: Task m Int
choose3 = choose2 <|> view 3

pick1 :: Task m Int
pick1 = empty <?> view 0

pick2 :: Task m Int
pick2 = view 1 <?> view 2

pick3 :: Task m Int
pick3 = pick2 <?> view 3


-- Guards --

auto :: Task m Text
auto = do
  x <- enter
  if x >= (10 :: Int)
    then view "large"
    else empty

actions :: Task m Int
actions = do
  _ <- enter :: Task m Int
  pick3

guards :: Task m Text
guards = do
  x <- enter
  (if x >= (10 :: Int) then view "large" else empty)
    <?> (if x >= (100 :: Int) then view "very large" else empty)

branch :: Task m Text
branch = do
  x <- update (1 :: Int)
  if x `mod` 3 == 0 then
    view "multiple of 3"
  else if x `mod` 5 == 0 then
    view "multiple of 5"
  else
    empty

branch' :: Task m Text
branch' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else empty)
    <|> (if x `mod` 5 == 0 then view "multiple of 5" else empty)

branch'' :: Task m Text
branch'' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else empty)
    <?> (if x `mod` 5 == 0 then view "multiple of 5" else empty)


-- Shared Data --

update1 :: Collaborative l m => l Int -> Task m ()
update1 l = do
  x <- enter
  l <<- x

update2 :: Collaborative l m => l Int -> Task m Int
update2 l = do
  x <- enter
  l <<- x
  y <- enter
  l <<- y
  watch l

update3 :: Collaborative l m => l Int -> Task m Int
update3 l = do
  x <- enter
  l <<- x
  y <- enter
  z <- watch l
  l <<- y + z
  watch l

inspect :: Collaborative l m => (l Int -> Task m a) -> Task m ( a, Int )
inspect f = do
  l <- share 0
  f l <&> watch l

doubleShared :: Collaborative l m => Task m ( (), Int )
doubleShared = do
  l <- share (0 :: Int)
  m <- share (0 :: Int)
  t2 l m <&> t1 l m
  where
    t1 _ m = do
      x <- change m
      if x >= 10 then view (x * 2) else empty
    t2 l m = do
      y <- change l
      if y >= 5 then m <<- 12 else empty

atomic :: Collaborative l m => Task m ( () , () )
atomic = do
  l <- share (0 :: Int)
  let
    t1 = do
      l <<- 2
      x <- watch l
      if x > 2 then view () else empty
    t2 = do
      l <<- 3
      l <<- 1
      view ()
  t1 <&> t2

unfixated :: Collaborative l m => Task m ( (), () )
unfixated = do
  l <- share False
  let
    t1 = do
      b <- watch l
      if b then view () else empty
    t2 = do
      l <<- True
      view ()
  t1 <&> t2


-- Forever --

numbers :: Collaborative l m => Task m ( Void, List Int )
numbers = do
  l <- share ([] :: List Int)
  forever (prepend l) <&> watch l
  where
    prepend l = do
      x <- enter
      l <<= (:) x

numbers' :: Collaborative l m => Task m ( Void, List Int )
numbers' = do
  l <- share ([] :: List Int)
  forever (edit_ l) <&> watch l
  where

    edit_ l = do pick
      [ ( "Prepend", prepend_ l )
      , ( "Clear", clear_ l )
      , ( "Change", change_ l )
      ]

    prepend_ l = do
      x <- enter
      l <<= (:) x

    clear_ l = do
      l <<- []

    change_ l = do
      --NOTE: `watch` should be before the external step, otherwise we'll end up an a state where we show an editor with the list when the user entered an improper index.
      -- Compare this with iTasks: `watch` should be before the external step because you cannot specify the `watch` inside the step list!
      n <- map length <| watch l
      i <- enter
      if i < n
        then pick
          [ ( "Delete", delete_ l i )
          , ( "Replace", replace_ l i )
          ]
        else empty

    delete_ l i =
      l <<= del i

    replace_ l i = do
      x <- enter
      l <<= rep i x


-- Helpers ---------------------------------------------------------------------

del :: Int -> List a -> List a
del _ []       = []
del 0 (_ : xs) = xs
del n (x : xs)
  | n >= 0     = x : del (pred n) xs
  | otherwise  = []

rep :: Int -> a -> List a -> List a
rep _ _ []       = []
rep 0 y (_ : xs) = y : xs
rep n y (x : xs)
  | n >= 0       = x : rep (pred n) y xs
  | otherwise    = []
