module Data.Someref
  ( Someref,
    pack,
    unpack,
  )
where

data Someref (m :: Type -> Type) where
  Someref :: (Typeable (r a), Eq (r a)) => r a -> Someref m

pack :: forall m r a. (Typeable (r a), Eq (r a)) => r a -> Someref m
pack = Someref

unpack :: forall m r a. (Typeable (r a)) => Someref m -> Maybe (r a)
unpack (Someref x)
  | Just Refl <- x ~: r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (r a)

instance Eq (Someref m) where
  Someref x == Someref y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
