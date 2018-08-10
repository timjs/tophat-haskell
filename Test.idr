module Test

import Data.Heap.Untyped



-- Universe --------------------------------------------------------------------


data Basic
  = INT
  | STR


Uninhabited (INT = STR) where
  uninhabited Refl impossible


DecEq Basic where
  decEq INT INT = Yes Refl
  decEq STR STR = Yes Refl

  decEq INT STR = No absurd
  decEq STR INT = No (negEqSym absurd)


[basic] Universe Basic where
  typeOf INT = Int
  typeOf STR = String

  defaultOf INT = 0
  defaultOf STR = ""



-- Tests -----------------------------------------------------------------------


test : Ref @{Test.basic} Int
test = do
  r <- ref (the Int 5)
  r := (the Int 10)
  x <- deref r
  pure (x + 2)
