module Data.Task.Input
  ( Path(..), Action(..), Input(..)
  , strip, (~?)
  ) where


import Preload

import Data.Task.Internal

infix 6 ~?



-- Paths -----------------------------------------------------------------------


data Path
  = GoLeft
  | GoRight
  deriving (Eq)


instance Pretty Path where
  pretty GoLeft  = "l"
  pretty GoRight = "r"



-- Inputs and Actions ----------------------------------------------------------


data Action :: Type where
  Change :: Basic b => Maybe b -> Action
  Empty :: Action
  Pick :: Path -> Action
  -- PickWith :: Label -> Action
  Continue :: Action
  -- ContinueWith :: Label -> Action


instance Eq Action where
  Change x == Change y
    | Just Refl <- sameT x y = x == y
    | otherwise = False
  Empty == Empty = True
  Pick x == Pick y = x == y
  -- PickWith x == PickWith y = x == y
  Continue == Continue = True
  -- ContinueWith x  == ContinueWith y = x == y
  _ == _ = False


instance Pretty Action where
  pretty (Change _)       = "change <val>"
  pretty (Empty)          = "empty"
  pretty (Pick p)         = "pick" <> pretty p
  -- pretty (PickWith l)     = "pick " <> l
  pretty (Continue)       = "cont"
  -- pretty (ContinueWith l) = "cont " <> l


data Input
  = ToLeft Input
  | ToHere Action
  | ToRight Input
  deriving (Eq)


instance Pretty Input where
  pretty (ToLeft e)  = "l " <> pretty e
  pretty (ToHere a)   = pretty a
  pretty (ToRight e) = "r " <> pretty e



-- Conformance -----------------------------------------------------------------


-- | A GADT representing some basic value `b` or no value at all.
data Stripped :: Type where
  Some :: Basic b => b -> Stripped
  None :: Stripped


instance Eq Stripped where
  Some x == Some y
    | Just Refl <- sameT x y = x == y
    | otherwise = False
  None == None = True
  _ == _ = False


strip :: Input -> Stripped
strip (ToHere (Change (Just v))) = Some v
strip (ToHere (Change Nothing))  = None
strip (ToHere _)                 = None
strip (ToLeft i)                 = strip i
strip (ToRight i)                = strip i


(~?) :: Input -> Input -> Bool
i1 ~? i2 = strip i1 == strip i2
