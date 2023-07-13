module Main where

import qualified Data.List as List
import Task
import Task.Interact
import Prelude hiding (repeat)

main :: IO ()
main =
  taskToIO booking

---- Examples ------------------------------------------------------------------
---- NOTE: Tasks ending with a quote need user input

---- Basics

enterInt :: Task h Int
enterInt = enter

fourtytwo :: Task h Int
fourtytwo =
  update 42

hello :: Task h Text
hello =
  update "Hello"

inc :: Int -> Task h Int
inc x =
  view (x + 1)

dec :: Int -> Task h Int
dec x =
  view (x - 1)

add :: Int -> Int -> Task h Int
add x y =
  view (x + y)

append :: Text -> Text -> Task h Text
append x y =
  view (x ++ y)

{-
---- Steps

pureStep :: Task h Int
pureStep = do
  x <- fourtytwo
  inc x

pureStep' :: Task h Int
pureStep' = do
  x <- fourtytwo
  inc x <?> fail

oneStep :: Task h Int
oneStep = do
  x <- update 0
  inc x

oneStep' :: Task h Int
oneStep' =
  update 0 >>? \x ->
    inc x

oneStep'' =
  update 0
    >>* ["Continue" ~> \x -> inc x]

-- oneStep'' :: Task h Int
-- oneStep'' = do
--   x <- update 0
--   select
--     [ "Increase" ~> inc x,
--       "Decrease" ~> dec x
--     ]

oneStep''' :: Task h Int
oneStep''' =
  update 0
    >>* [ "Increase" ~> inc,
          "Decrease" ~> dec
        ]

twoSteps :: Task h Int
twoSteps = do
  x <- update 1
  y <- update 2
  add x y

twoSteps' :: Task h Int
twoSteps' =
  update 1 >>? \x ->
    update 2 >>? \y ->
      add x y

twoSteps'' :: Task h Int
twoSteps'' = do
  x <- enter
  y <- enter
  add x y

twoSteps''' :: Task h Int
twoSteps''' =
  enter >>? \x ->
    enter >>? \y ->
      add x y

---- Parallel

parallelSimple :: Task h (Int, Text)
parallelSimple = enter >< hello

parallelStep :: Task h Text
parallelStep = do
  (n, m) <- parallelSimple
  view (unwords <| replicate n m)

parallelStep' :: Task h Text
parallelStep' = do
  (n, m) <- parallelSimple
  view (unwords <| replicate n m) <?> fail

parallelStep'' :: Task h Text
parallelStep'' =
  parallelSimple >>? \(n, m) ->
    view (unwords <| replicate n m)

parallelTest :: Task h (List Int)
parallelTest = do
  xs <- parallel [enterInt, enterInt, enterInt]
  view xs

---- Choices

choose1 :: Task h Int
choose1 = fail <|> view 1

choose2 :: Task h Int
choose2 = view 1 <|> view 2

choose3 :: Task h Int
choose3 = choose2 <|> view 3

pick1 :: Task h Int
pick1 = fail <?> view 1

pick2 :: Task h Int
pick2 = view 1 <?> view 2

pick3 :: Task h Int
pick3 = pick2 <?> view 3

pick3' :: Task h Int
pick3' =
  pick
    [ "A" ~> view 1,
      "B" ~> view 2,
      "C" ~> view 3
    ]

parpick :: Task h (Int, Int)
parpick = pick3' >< pick3'

---- Guards

auto :: Task h Text
auto = do
  x <- enter
  if x >= (10 :: Int)
    then view "large"
    else fail

actions :: Task h Int
actions = do
  _ <- enter :: Task h Int
  pick3

actions' :: Task h Int
actions' = do
  _ <- enter :: Task h Int
  pick3'

actions'' :: Task h Int
actions'' = do
  enter
    >>* [ "A" ~> (\x -> view (x * 1 :: Int)),
          "B" ~> (\x -> view (x * 2 :: Int)),
          "C" ~> (\x -> view (x * 3 :: Int))
        ]

guards :: Task h Text
guards = do
  x <- enter
  pick
    [ "A" ~> if x >= (10 :: Int) then view "large" else fail,
      "B" ~> if x >= (100 :: Int) then view "very large" else fail
    ]

guards' :: Task h Text
guards' = do
  enter
    >>* [ "A" ~> \x -> if x >= (10 :: Int) then view "large" else fail,
          "B" ~> \x -> if x >= (100 :: Int) then view "very large" else fail
        ]

branch :: Task h Text
branch = do
  x <- update (1 :: Int)
  if x `mod` 3 == 0
    then view "multiple of 3"
    else
      if x `mod` 5 == 0
        then view "multiple of 5"
        else fail

branch' :: Task h Text
branch' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else fail)
    <|> (if x `mod` 5 == 0 then view "multiple of 5" else fail)

branch'' :: Task h Text
branch'' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else fail)
    <?> (if x `mod` 5 == 0 then view "multiple of 5" else fail)

branch''' :: Task h Text
branch''' = do
  update (1 :: Int)
    >>* [ "Div3" ~> \x -> if x `mod` 3 == 0 then view "multiple of 3" else fail,
          "Div5" ~> \x -> if x `mod` 5 == 0 then view "multiple of 5" else fail
        ]

---- Future

enterFutureR :: Task h Int
enterFutureR = enterInt >>= \_ -> (pick ["A" ~> view (1 :: Int)] >>= \_ -> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)])

enterFutureL :: Task h Int
enterFutureL = (enterInt >>= \_ -> pick ["A" ~> view (1 :: Int)]) >>= \_ -> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

updateFutureR :: Task h Int
updateFutureR = fourtytwo >>= \_ -> (pick ["A" ~> view (1 :: Int)] >>= \_ -> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)])

updateFutureL :: Task h Int
updateFutureL = (fourtytwo >>= \_ -> pick ["A" ~> view (1 :: Int)]) >>= \_ -> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

mixedFutureR :: Task h Int
mixedFutureR = enterInt >>= (\_ -> fourtytwo >>= \_ -> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)])

mixedFutureL :: Task h Int
mixedFutureL = (enterInt >>= \_ -> fourtytwo) >>= \_ -> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

parallelFuture :: Task h (Int, Int)
parallelFuture = do
  _ <- enterInt
  pick ["A" ~> view (1 :: Int)] >< pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

troubledFuture :: Task h (Int, Int)
troubledFuture =
  t >< t
  where
    t = view (0 :: Int) >>= \_ -> pick ["A" ~> view (42 :: Int)]

nestedFuture :: Task h Int
nestedFuture = do
  _ <- enterInt
  pick
    [ "A" ~> view (1 :: Int),
      "B" ~> pick ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]
    ]

---- Shared Data

update1 :: Store h Int -> Task h ()
update1 r = do
  x <- enter
  r <<- x

update2 :: Store h Int -> Task h Int
update2 r = do
  x <- enter
  r <<- x
  y <- enter
  r <<- y
  watch r

update3 :: Store h Int -> Task h Int
update3 r = do
  x <- enter
  r <<- x
  y <- enter
  z <- watch r
  r <<- y + z
  watch r

inspect :: (Reflect h) => (Store h Int -> Task h a) -> Task h (a, Int)
inspect f = do
  r <- share 0
  f r >< watch r

doubleShared :: (Reflect h) => Task h ((), Int)
doubleShared = do
  r <- share (0 :: Int)
  m <- share (0 :: Int)
  t2 r m >< t1 r m
  where
    t1 _ m = do
      x <- change m
      if x >= 10 then view (x * 2) else fail
    t2 r m = do
      y <- change r
      if y >= 5 then m <<- 12 else fail

atomic :: (Reflect h) => Task h ((), ())
atomic = do
  r <- share (0 :: Int)
  let t1 = do
        r <<- 2
        x <- watch r
        if x > 2 then view () else fail
      t2 = do
        r <<- 3
        r <<- 1
        view ()
  -- t1 >< t2
  t2 >< t1

unfixated :: (Reflect h) => Task h ((), ())
unfixated = do
  r <- share False
  let t1 = do
        b <- watch r
        if b then view () else fail
      t2 = do
        r <<- True
        view ()
  t1 >< t2

---- Forever

numbers :: (Reflect h) => Task h (Void, List Int)
numbers = do
  r <- share ([] :: List Int)
  forever (prepend r) >< watch r
  where
    prepend r = do
      x <- enter
      r <<= (:) x

numbers' :: (Reflect h) => Task h (List Int)
numbers' = do
  r <- share ([] :: List Int)
  ((), ns) <- repeat (edit_ r >< watch r)
  view ns
  where
    edit_ r =
      pick
        [ ("Prepend", prepend_ r),
          ("Clear", clear_ r),
          ("Change", change_ r)
        ]
    prepend_ r = do
      x <- enter
      r <<= (:) x
    clear_ r = do
      r <<- []
    change_ r = do
      --NOTE: `watch` should be before the external step, otherwise we'll end up an a state where we show an editor with the list when the user entered an improper index.
      -- Compare this with iTasks: `watch` should be before the external step because you cannot specify the `watch` inside the step list!
      n <- map (length >> fromIntegral) (watch r)
      i <- enter
      pick
        [ "Delete" ~> if i < n then delete_ r i else fail,
          "Replace" ~> if i < n then replace_ r i else fail
        ]
    delete_ r i =
      r <<= del i
    replace_ r i = do
      x <- enter
      r <<= rep i x

---- Helpers -------------------------------------------------------------------

del :: Int -> List a -> List a
del _ [] = []
del 0 (_ : xs) = xs
del n (x : xs)
  | n >= 0 = x : del (pred n) xs
  | otherwise = []

rep :: Int -> a -> List a -> List a
rep _ _ [] = []
rep 0 y (_ : xs) = y : xs
rep n y (x : xs)
  | n >= 0 = x : rep (pred n) y xs
  | otherwise = []
 -}

---- Flight booking ------------------------------------------------------------

type Name = Text

type Age = Nat

type Passenger = (Name, Age)

type Passengers = List Passenger

type Seat = (Nat, Char)

type Seats = List Seat

booking :: (Typeable h) => Task h Seats
booking = do
  free <- share [(r, s) | r <- [1 .. 10], s <- ['A' .. 'D']]
  watch free <& pool (book free)

book :: Store h Seats -> Task h (Passengers, Seats)
book free = do
  ps <- enter' "Passenger details"
  if allValid ps
    then chooseSeats ps free
    else fail

allValid :: Passengers -> Bool
allValid ps = all isValid ps && any isAdult ps

isValid :: Passenger -> Bool
isValid (n, a) = n /= "" && a >= 0

isAdult :: Passenger -> Bool
isAdult (_, a) = a >= 18

chooseSeats :: Passengers -> Store h Seats -> Task h (Passengers, Seats)
chooseSeats ps free = do
  -- Warn: Be aware of the need of the parallel here! Will otherwise hang on the step after the watch.
  (fs, ss) <- watch free <&> enter' "Seats selection"
  if isCorrect ss fs && length ps == length ss
    then confirmBooking ps ss free
    else fail

isCorrect :: Seats -> Seats -> Bool
isCorrect ss fs = do
  List.intersect ss fs == ss

confirmBooking :: Passengers -> Seats -> Store h Seats -> Task h (Passengers, Seats)
confirmBooking ps ss free = do
  free <<= difference ss
  view (ps, ss)

difference :: (Eq a) => List a -> List a -> List a
difference = flip <| foldr List.delete
