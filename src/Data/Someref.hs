module Data.Someref
  ( Someref,
    pack,
    unpack,
  )
where

import Data.Heap (Heap, Ref)

data Someref (h :: Heap h') where
  Someref :: (Typeable (Ref h a), Eq (Ref h a)) => Ref h a -> Someref h

pack :: forall h a. (Typeable (Ref h a), Eq (Ref h a)) => Ref h a -> Someref h
pack = Someref

unpack :: forall h a. (Typeable (Ref h a)) => Someref h -> Maybe (Ref h a)
unpack (Someref x)
  | Just Refl <- x ~: r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (Ref h a)

instance Eq (Someref h) where
  Someref x == Someref y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
