module Main where


-- import Data.Task
-- import Data.Task.Run

main :: IO ()
main =
  putStrLn "Hello!"

{-
-- Examples --------------------------------------------------------------------
--
-- NOTE: Tasks ending with a quote need user input
--


-- Basics --


fourtytwo :: Task Int
fourtytwo =
  edit 42


hello :: Task Text
hello =
  edit "Hello"


inc :: Int -> Task Int
inc x =
  edit (x + 1)


add :: Int -> Int -> Task Int
add x y =
  edit (x + y)


append :: Text -> Text -> Task Text
append x y =
  edit (x <> y)



-- Steps --


pureStep :: Task Int
pureStep =
  fourtytwo >>- \x ->
  inc x


pureStep' :: Task Int
pureStep' =
    fourtytwo >>? \x ->
    inc x



pureStep'' :: Task Int
pureStep'' =
  fourtytwo >>? \x ->
  inc x


oneStep :: Task Int
oneStep =
  edit 0 >>- \x ->
  inc x


oneStep' :: Task Int
oneStep' =
  edit 0 >>? \x ->
  inc x


twoSteps :: Task Int
twoSteps =
  edit 1 >>- \x ->
  edit 2 >>- \y ->
  add x y


twoSteps' :: Task Int
twoSteps' =
  edit 1 >>? \x ->
  edit 2 >>? \y ->
  add x y



-- Parallel --


parallel :: Task ( Int, Text )
parallel = enter -&&- hello


parallelStep :: Task Text
parallelStep =
  parallel >>- \( n, m ) ->
  edit (unwords $ replicate n m)


parallelStep' :: Task Text
parallelStep' =
  parallel >>? \( n, m ) ->
  edit (unwords $ replicate n m)



-- Normalisation --
--
-- FIXME: should these automatically simplify?


pair :: Task ( Int, Int )
pair = edit 3 -&&- edit 8


inner :: Task Int
inner =
  pair >>- \( x, y ) ->
  add x y


inner' :: Task Int
inner' =
  pair >>? \( x, y ) ->
  add x y



{- Shared Data --

{-
partial
editList :: Task ( (), (LIST Int) )
editList = do
  l <- ref (LIST Int) []
  start l -&&- watch l

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
        "Delete" -#- delete l i -??- "Replace" -#- replace l i
      else
        failure

    prepend :: Loc (LIST Int) -> Task ()
    prepend l =
      "Give a new value to prepend" -#- enter >>? \x ->
      modify (LIST Int) l ((::) x)

    clear :: Loc (LIST Int) -> Task ()
    clear l =
      modify (LIST Int) l (const [])

    quit :: Task ()
    quit = edit ()

    mutual
      partial
      repeat :: Loc (LIST Int) -> Task ()
      repeat l = do
        "Prepend" -#- prepend l -??- "Clear" -#- clear l -??- "Change" -#- change l
        start l

      partial
      start :: Loc (LIST Int) -> Task ()
      start l =
        "Edit" -#- repeat l -??- "Quit" -#- quit
-}

update1 :: Loc Int -> Task Int
update1 l = do
  n <- enter
  assign Int l n
  m <- enter
  modify Int l ((+) m)
  edit !(deref l)


update2 :: Loc Int -> Task ()
update2 l =
  deref l >>= \x ->
  edit (x + 1) >>? \y ->
  l $= const y >>= \() ->
  deref l >>= \u ->
  edit (u + 2) >>? \v ->
  l $= const v


inspect :: Show (typeOf a) -> (Loc Int -> Task a) -> Task ( a, Int )
inspect f = do
  l <- Int 0
  f l -&&- watch l

-- inspect :: Show (typeOf a) -> Show (typeOf b) -> (Loc b -> Task a) -> Task ( a, b )
-- inspect {b} f = do
--   l <- init b
--   f l -&&- watch l


doubleShared :: Task ( (), Int )
doubleShared = do
  l <- ref 0
  m <- ref 0
  let t1 = do
    x <- watch m
    if x >= 10 then edit (x * 2) else failure
  let t2 = do
    y <- watch l
    if y >= 5 then m $= const 12 else failure
  t2 -&&- t1



-- Choices --


pick1 :: Task Int
pick1 = failure -||- edit 0


pick2 :: Task Int
pick2 = edit 1 -||- edit 2


pick3 :: Task Int
pick3 = pick2 -||- edit 3


pick1' :: Task Int
pick1' = "Fail" -#- failure -??- "Cont" -#- edit 0


pick2' :: Task Int
pick2' = "Pick one of two" -#- ("First" -#- edit 1 -??- "Second" -#- edit 2)


pick3' :: Task Int
pick3' = "Pick one of three" -#- (pick2' -??- "Third" -#- edit 3)



-- Guards --


auto :: Task Text
auto = do
  x <- enter
  if x >= 10 then edit "large" else failure


actions :: Task Int
actions =
  enter >>? \x ->
  pick3


actions' :: Task Int
actions' =
  enter >>? \x ->
  pick3'


guards :: Task Text
guards = do
  x <- enter
  "Large" -#- (if x >= 10 then edit "large" else failure)
    -??-
    "VeryLarge" -#- (if x >= 100 then edit "very large" else failure)


guards' :: Task Text
guards' = do
  enter >>? \x ->
  ("Large" -#- (if x >= 10 then edit "large" else failure)
    -??-
    "VeryLarge" -#- (if x >= 100 then edit "very large" else failure))


branch :: Task Text
branch =
  edit 1 >>? \x ->
  if x `mod` 3 == 0 then
    edit "multiple of 3"
  else if x `mod` 5 == 0 then
    edit "multiple of 5"
  else
    failure



-- Empty edit --


empties :: Task Int
empties = do
  ( x, y ) <- enter -&&- enter
  edit (x + y)




-}


main :: IO ()
main =
  run parallelStep'
-}
