module Data.Task.Input where


import Preload

import Data.Surely

import Data.Task.Internal



-- infix 6 =~, /~



-- Paths -----------------------------------------------------------------------


data Path
  = GoLeft
  | GoRight
  deriving (Show, Eq)


-- Events ----------------------------------------------------------------------


data Action b
  = Change (Surely b)
  | Empty
  | Pick Path
  | PickWith Label
  | Continue
  | ContinueWith Label
  deriving (Show, Eq)


data Input b
  = ToLeft (Input b)
  | ToHere (Action b)
  | ToRight (Input b)
  deriving (Show, Eq)



{- Conformance -----------------------------------------------------------------


strip : (i : Input) -> Maybe (c : Ty ** typeOf c)
strip (ToHere (Change {c} (Exactly v))) = Just (c ** v)
strip (ToHere (Change Anything))        = Nothing
strip (ToHere _)                        = Nothing
strip (ToLeft i)                        = strip i
strip (ToRight i)                       = strip i


(=~) : Input -> Input -> Bool
--FIXME: Why does a with-view not work?
i1 =~ i2 = case ( strip i1, strip i2 ) of
  ( Just (c1 ** v1), Just (c2 ** v2) ) => case decEq c1 c2 of
    Yes Refl => case eq c1 of
      eq_c1 => v1 == v2
    No contr => False
  _ => True


(/~) : Input -> Input -> Bool
i1 /~ i2 = not (i1 =~ i2)


-------------------------------------------------------------------------------}
