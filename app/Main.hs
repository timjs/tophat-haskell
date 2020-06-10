module Main where

import Data.Task
import Data.Task.Interact

main :: IO ()
main =
  taskToIO (add 3 4)

-- Examples --------------------------------------------------------------------
--
-- NOTE: Tasks ending with a quote need user input
--

-- Basics --

enterInt :: Task h r Int
enterInt = enter

fourtytwo :: Task h r Int
fourtytwo =
  update 42

hello :: Task h r Text
hello =
  update "Hello"

inc :: Int -> Task h r Int
inc x =
  view (x + 1)

dec :: Int -> Task h r Int
dec x =
  view (x - 1)

add :: Int -> Int -> Task h r Int
add x y =
  view (x + y)

append :: Text -> Text -> Task h r Text
append x y =
  view (x ++ y)

-- Steps --

pureStep :: Task h r Int
pureStep = do
  x <- fourtytwo
  inc x

pureStep' :: Task h r Int
pureStep' = do
  x <- fourtytwo
  inc x <?> fail

oneStep :: Task h r Int
oneStep = do
  x <- update 0
  inc x

oneStep' :: Task h r Int
oneStep' =
  update 0 >>? \x ->
    inc x

oneStep'' :: Task h r Int
oneStep'' = do
  x <- update 0
  select
    [ "Increase" ~> inc x,
      "Decrease" ~> dec x
    ]

oneStep''' :: Task h r Int
oneStep''' =
  update 0
    >>* [ "Increase" ~> inc,
          "Decrease" ~> dec
        ]

twoSteps :: Task h r Int
twoSteps = do
  x <- update 1
  y <- update 2
  add x y

twoSteps' :: Task h r Int
twoSteps' =
  update 1 >>? \x ->
    update 2 >>? \y ->
      add x y

twoSteps'' :: Task h r Int
twoSteps'' = do
  x <- enter
  y <- enter
  add x y

twoSteps''' :: Task h r Int
twoSteps''' =
  enter >>? \x ->
    enter >>? \y ->
      add x y

-- Parallel --

parallelSimple :: Task h r (Int, Text)
parallelSimple = enter >< hello

parallelStep :: Task h r Text
parallelStep = do
  (n, m) <- parallelSimple
  view (unwords <| replicate n m)

parallelStep' :: Task h r Text
parallelStep' = do
  (n, m) <- parallelSimple
  view (unwords <| replicate n m) <?> fail

parallelStep'' :: Task h r Text
parallelStep'' =
  parallelSimple >>? \(n, m) ->
    view (unwords <| replicate n m)

parallelTest :: Task h r (List Int)
parallelTest = do
  xs <- parallel [enterInt, enterInt, enterInt]
  view xs

-- Choices --

choose1 :: Task h r Int
choose1 = fail <|> view 1

choose2 :: Task h r Int
choose2 = view 1 <|> view 2

choose3 :: Task h r Int
choose3 = choose2 <|> view 3

pick1 :: Task h r Int
pick1 = fail <?> view 1

pick2 :: Task h r Int
pick2 = view 1 <?> view 2

pick3 :: Task h r Int
pick3 = pick2 <?> view 3

pick3' :: Task h r Int
pick3' =
  select
    [ "A" ~> view 1,
      "B" ~> view 2,
      "C" ~> view 3
    ]

parpick :: Task h r (Int, Int)
parpick = pick3' >< pick3'

-- Guards --

auto :: Task h r Text
auto = do
  x <- enter
  if x >= (10 :: Int)
    then view "large"
    else fail

actions :: Task h r Int
actions = do
  _ <- enter :: Task h r Int
  pick3

actions' :: Task h r Int
actions' = do
  _ <- enter :: Task h r Int
  pick3'

actions'' :: Task h r Int
actions'' = do
  enter
    >>* [ "A" ~> (\x -> view (x * 1 :: Int)),
          "B" ~> (\x -> view (x * 2 :: Int)),
          "C" ~> (\x -> view (x * 3 :: Int))
        ]

guards :: Task h r Text
guards = do
  x <- enter
  select
    [ "A" ~> if x >= (10 :: Int) then view "large" else fail,
      "B" ~> if x >= (100 :: Int) then view "very large" else fail
    ]

guards' :: Task h r Text
guards' = do
  enter
    >>* [ "A" ~> \x -> if x >= (10 :: Int) then view "large" else fail,
          "B" ~> \x -> if x >= (100 :: Int) then view "very large" else fail
        ]

branch :: Task h r Text
branch = do
  x <- update (1 :: Int)
  if x `mod` 3 == 0
    then view "multiple of 3"
    else
      if x `mod` 5 == 0
        then view "multiple of 5"
        else fail

branch' :: Task h r Text
branch' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else fail)
    <|> (if x `mod` 5 == 0 then view "multiple of 5" else fail)

branch'' :: Task h r Text
branch'' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else fail)
    <?> (if x `mod` 5 == 0 then view "multiple of 5" else fail)

branch''' :: Task h r Text
branch''' = do
  x <- update (1 :: Int)
  select
    [ "Div3" ~> if x `mod` 3 == 0 then view "multiple of 3" else fail,
      "Div5" ~> if x `mod` 5 == 0 then view "multiple of 5" else fail
    ]

-- Future --

enterFutureR :: Task h r Int
enterFutureR = enterInt >>= \_ -> (select ["A" ~> view (1 :: Int)] >>= \_ -> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)])

enterFutureL :: Task h r Int
enterFutureL = (enterInt >>= \_ -> select ["A" ~> view (1 :: Int)]) >>= \_ -> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

updateFutureR :: Task h r Int
updateFutureR = fourtytwo >>= \_ -> (select ["A" ~> view (1 :: Int)] >>= \_ -> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)])

updateFutureL :: Task h r Int
updateFutureL = (fourtytwo >>= \_ -> select ["A" ~> view (1 :: Int)]) >>= \_ -> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

mixedFutureR :: Task h r Int
mixedFutureR = (enterInt >>= \_ -> fourtytwo) >>= \_ -> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

mixedFutureL :: Task h r Int
mixedFutureL = (enterInt >>= \_ -> fourtytwo) >>= \_ -> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

parallelFuture :: Task h r (Int, Int)
parallelFuture = do
  _ <- enterInt
  select ["A" ~> view (1 :: Int)] >< select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]

nestedFuture :: Task h r Int
nestedFuture = do
  _ <- enterInt
  select
    [ "A" ~> view (1 :: Int),
      "B" ~> select ["A" ~> view (2 :: Int), "B" ~> view (3 :: Int)]
    ]

-- Shared Data --

update1 :: Store h Int -> Task h r ()
update1 r = do
  x <- enter
  r <<- x

update2 :: Store h Int -> Task h r Int
update2 r = do
  x <- enter
  r <<- x
  y <- enter
  r <<- y
  watch r

update3 :: Store h Int -> Task h r Int
update3 r = do
  x <- enter
  r <<- x
  y <- enter
  z <- watch r
  r <<- y + z
  watch r

inspect :: (Inspect h Int) => (Store h Int -> Task h r a) -> Task h r (a, Int)
inspect f = do
  r <- share 0
  f r >< watch r

doubleShared :: (Inspect h Int) => Task h r ((), Int)
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

atomic :: (Inspect h Int) => Task h r ((), ())
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

unfixated :: (Inspect h Bool) => Task h r ((), ())
unfixated = do
  r <- share False
  let t1 = do
        b <- watch r
        if b then view () else fail
      t2 = do
        r <<- True
        view ()
  t1 >< t2

-- Forever --

numbers :: (Inspect h (List Int)) => Task h r (Void, List Int)
numbers = do
  r <- share ([] :: List Int)
  forever (prepend r) >< watch r
  where
    prepend r = do
      x <- enter
      r <<= (:) x

numbers' :: (Inspect h (List Int)) => Task h r (List Int)
numbers' = do
  r <- share ([] :: List Int)
  (edit_ r >< watch r) >>@ exit_
  where
    exit_ ((), ns) =
      view ns
    edit_ r =
      do select
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
      select
        [ "Delete" ~> if i < n then delete_ r i else fail,
          "Replace" ~> if i < n then replace_ r i else fail
        ]
    delete_ r i =
      r <<= del i
    replace_ r i = do
      x <- enter
      r <<= rep i x

-- Helpers ---------------------------------------------------------------------

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
