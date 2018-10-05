module Data.Task.Input
  ( Path(..), Action(..), Input(..)
  , strip, (~?)
  , module Data.Surely
  ) where


import Preload

import Data.Surely

import Data.Task.Internal



infix 6 ~?



-- Paths -----------------------------------------------------------------------


data Path
  = GoLeft
  | GoRight
  deriving (Show, Eq)


-- Inputs and Actions ----------------------------------------------------------


data Action b
  = Change (Surely b)
  | Empty
  | Pick Path
  -- | PickWith Label
  | Continue
  -- | ContinueWith Label
  deriving (Show, Eq)


data Input b
  = ToLeft (Input b)
  | ToHere (Action b)
  | ToRight (Input b)
  deriving (Show, Eq)



-- Conformance -----------------------------------------------------------------


strip :: Input b -> Maybe b
strip (ToHere (Change (Exactly v))) = Just v
strip (ToHere (Change Anything))    = Nothing
strip (ToHere _)                    = Nothing
strip (ToLeft i)                    = strip i
strip (ToRight i)                   = strip i


(~?) :: Basic b => Input b -> Input b -> Bool
i1 ~? i2
  | ( Just v1, Just v2 ) <- ( strip i1, strip i2 ) = v1 == v2
  | otherwise = True
