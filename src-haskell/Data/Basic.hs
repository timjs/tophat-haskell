module Data.Basic
  ( Basic
  , Somebasic, pack, unpack
  ) where


import Preload

import Type.Reflection

import Test.QuickCheck (Arbitrary)

import qualified GHC.Show as Show



-- Class -----------------------------------------------------------------------


type Basic a = ( Show a, Eq a, Typeable a, Arbitrary a ) -- Coarbitrary a )



-- Packing and unpacking -------------------------------------------------------


data Somebasic :: Type where
  Somebasic :: forall a. Basic a => TypeRep a -> a -> Somebasic


pack :: forall a. Basic a => a -> Somebasic
pack x = Somebasic typeRep x


unpack :: forall a. Basic a => Somebasic -> Maybe a
unpack (Somebasic r x)
  | Just HRefl <- eqTypeRep r r' = Just x
  | otherwise = Nothing
  where
    r' = typeRep :: TypeRep a



-- Instances -------------------------------------------------------------------


instance Show Somebasic where
  show (Somebasic _ x) = Show.show x


instance Eq Somebasic where
  (Somebasic r x) == (Somebasic s y)
    | Just HRefl <- eqTypeRep r s = x == y
    | otherwise = False
