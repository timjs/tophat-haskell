module Data.Task.Input
  ( Path(..), Action(..), Dummy(..), Input(..)
  , dummyfy, reify, strip, fill
  ) where


import Preload

import Test.QuickCheck (Gen, vectorOf)

import Data.Task.Internal



-- Paths -----------------------------------------------------------------------


data Path
  = GoLeft
  | GoRight
  deriving (Eq)


instance Pretty Path where
  pretty GoLeft  = "l"
  pretty GoRight = "r"



-- Inputs ----------------------------------------------------------------------

-- Real actions --


data Action :: Type where
  Change       :: Basic b => b -> Action
  Empty        :: Action
  Pick         :: Path -> Action
  Continue     :: Action
  -- PickWith     :: Label -> Action
  -- ContinueWith :: Label -> Action


instance Eq Action where
  Change x == Change y
    | Just Refl <- sameT x y = x == y
    | otherwise              = False
  Empty    == Empty          = True
  Pick x   == Pick y         = x == y
  Continue == Continue       = True
  _        == _              = False


instance Pretty Action where
  pretty (Change x) = "change " <> show x
  pretty (Empty)    = "empty"
  pretty (Pick p)   = "pick" <> pretty p
  pretty (Continue) = "cont"



-- Dummy actions --


data Dummy :: Type where
  AChange       :: Basic b => Proxy b -> Dummy
  AEmpty        :: Dummy
  APick         :: Path -> Dummy
  AContinue     :: Dummy
  -- APickWith     :: Label -> Dummy
  -- AContinueWith :: Label -> Dummy


instance Eq Dummy where
  AChange x == AChange y
    -- We're comparing proxies, they are always equal when the types are equal.
    | Just Refl <- sameT x y = True
    | otherwise              = False
  AEmpty    == AEmpty        = True
  APick x   == APick y       = x == y
  AContinue == AContinue     = True
  _         == _             = False


instance Pretty Dummy where
  pretty (AChange _) = "change <val>"
  pretty (AEmpty)    = "empty"
  pretty (APick p)   = "pick" <> pretty p
  pretty (AContinue) = "cont"



-- Inputs --


data Input a
  = ToLeft (Input a)
  | ToHere a
  | ToRight (Input a)
  deriving (Eq, Functor, Foldable, Traversable)


instance Pretty a => Pretty (Input a) where
  pretty (ToLeft e)  = "l " <> pretty e
  pretty (ToHere a)  = pretty a
  pretty (ToRight e) = "r " <> pretty e



-- Conformance -----------------------------------------------------------------


dummyfy :: Action -> Dummy
dummyfy (Change x) = AChange (proxyOf x)
dummyfy (Empty)    = AEmpty
dummyfy (Pick p)   = APick p
dummyfy (Continue) = AContinue


reify :: Dummy -> Gen (List Action)
reify (AChange p) = map Change <$> vectorOf 5 (arbitraryOf p)
reify (AEmpty)    = pure [ Empty ]
reify (APick p)   = pure [ Pick p ]
reify (AContinue) = pure [ Continue ]


strip :: Input Action -> Input Dummy
strip = map dummyfy

fill :: Input Dummy -> Gen (List (Input Action))
fill = map sequence << sequence << map reify
