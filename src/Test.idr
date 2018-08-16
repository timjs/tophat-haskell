module Test

import Data.Universe
import Control.Monad.Ref



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


test0 : IO Int
test0 = do
  r <- newIORef 5
  writeIORef r 10
  x <- readIORef r
  pure x


test1 : MonadRef IORef IO => IO Int
test1 = do
  r <- ref (the Int 5)
  r := (the Int 10)
  x <- deref r
  pure x


test2 : MonadRef IORef IO => IO String
test2 = do
  r <- ref "Hello"
  r := !(deref r) ++ " World!"
  x <- deref r
  pure x


-- testRef : Ref @{Test.basic} Int
-- testRef = do
--   r <- ref (the Int 5)
--   r := (the Int 10)
--   x <- deref r
--   pure (x + 2)
