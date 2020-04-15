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

dec :: Int -> Task m Int
dec x =
  view (x - 1)

add :: Int -> Int -> Task m Int
add x y =
  view (x + y)

append :: Text -> Text -> Task m Text
append x y =
  view (x ++ y)

-- Steps --

pureStep :: Task m Int
pureStep = do
  x <- fourtytwo
  inc x

pureStep' :: Task m Int
pureStep' = do
  x <- fourtytwo
  inc x <?> fail

oneStep :: Task m Int
oneStep = do
  x <- update 0
  inc x

oneStep' :: Task m Int
oneStep' =
  update 0 >>? \x ->
    inc x

oneStep'' :: Task m Int
oneStep'' = do
  x <- update 0
  inc x <?> dec x

oneStep''' :: Task m Int
oneStep''' =
  update 0
    >>* [ "Increase" ~> inc,
          "Decrease" ~> dec
        ]

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

twoSteps''' :: Task m Int
twoSteps''' =
  enter >>? \x ->
    enter >>? \y ->
      add x y

-- Parallel --

parallel :: Task m (Int, Text)
parallel = enter >< hello

parallelStep :: Task m Text
parallelStep = do
  (n, m) <- parallel
  view (unwords <| replicate n m)

parallelStep' :: Task m Text
parallelStep' = do
  (n, m) <- parallel
  view (unwords <| replicate n m) <?> fail

parallelStep'' :: Task m Text
parallelStep'' =
  parallel >>? \(n, m) ->
    view (unwords <| replicate n m)

-- Choices --

choose1 :: Task m Int
choose1 = fail <|> view 1

choose2 :: Task m Int
choose2 = view 1 <|> view 2

choose3 :: Task m Int
choose3 = choose2 <|> view 3

pick1 :: Task m Int
pick1 = fail <?> view 1

pick2 :: Task m Int
pick2 = view 1 <?> view 2

pick3 :: Task m Int
pick3 = pick2 <?> view 3

pick3' :: Task m Int
pick3' =
  select
    [ "A" ~> view 1,
      "B" ~> view 2,
      "C" ~> view 3
    ]

parpick :: Task m (Int, Int)
parpick = pick3' >< pick3'

parpick' :: Task m (Int, Int)
parpick' = do
  update (1 :: Int)
  parpick

-- Guards --

auto :: Task m Text
auto = do
  x <- enter
  if x >= (10 :: Int)
    then view "large"
    else fail

actions :: Task m Int
actions = do
  _ <- enter :: Task m Int
  pick3

actions' :: Task m Int
actions' = do
  _ <- enter :: Task m Int
  pick3'

actions'' :: Task m Int
actions'' = do
  enter
    >>* [ "A" ~> (\x -> view (x * 1 :: Int)),
          "B" ~> (\x -> view (x * 2 :: Int)),
          "C" ~> (\x -> view (x * 3 :: Int))
        ]

guards :: Task m Text
guards =
  enter
    >>* [ "A" ~> \x -> if x >= (10 :: Int) then view "large" else fail,
          "B" ~> \x -> if x >= (100 :: Int) then view "very large" else fail
        ]

branch :: Task m Text
branch = do
  x <- update (1 :: Int)
  if x `mod` 3 == 0
    then view "multiple of 3"
    else
      if x `mod` 5 == 0
        then view "multiple of 5"
        else fail

branch' :: Task m Text
branch' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else fail)
    <|> (if x `mod` 5 == 0 then view "multiple of 5" else fail)

branch'' :: Task m Text
branch'' = do
  x <- update (1 :: Int)
  (if x `mod` 3 == 0 then view "multiple of 3" else fail)
    <?> (if x `mod` 5 == 0 then view "multiple of 5" else fail)

branch''' :: Task m Text
branch''' =
  update (1 :: Int)
    >>* [ "Div3" ~> \x -> if x `mod` 3 == 0 then view "multiple of 3" else fail,
          "Div5" ~> \x -> if x `mod` 5 == 0 then view "multiple of 5" else fail
        ]

-- Shared Data --

update1 :: Collaborative r m => Store r Int -> Task m ()
update1 r = do
  x <- enter
  r <<- x

update2 :: Collaborative r m => Store r Int -> Task m Int
update2 r = do
  x <- enter
  r <<- x
  y <- enter
  r <<- y
  watch r

update3 :: Collaborative r m => Store r Int -> Task m Int
update3 r = do
  x <- enter
  r <<- x
  y <- enter
  z <- watch r
  r <<- y + z
  watch r

inspect :: Collaborative r m => (Store r Int -> Task m a) -> Task m (a, Int)
inspect f = do
  r <- share 0
  f r >< watch r

doubleShared :: Collaborative r m => Task m ((), Int)
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

atomic :: Collaborative r m => Task m ((), ())
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
  t1 >< t2

unfixated :: Collaborative r m => Task m ((), ())
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

numbers :: Collaborative r m => Task m (Void, List Int)
numbers = do
  r <- share ([] :: List Int)
  forever (prepend r) >< watch r
  where
    prepend r = do
      x <- enter
      r <<= (:) x

numbers' :: Collaborative r m => Task m (List Int)
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
      enter
        >>* [ "Delete" ~> \i -> if i < n then delete_ r i else fail,
              "Replace" ~> \i -> if i < n then replace_ r i else fail
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
