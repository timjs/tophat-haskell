module Data.Basic
  ( Basic, Prim
  , Somebasic, pack, unpack, unsafeUnpack
  ) where



-- Basics ----------------------------------------------------------------------


type Basic a = ( Pretty a, Read a, Eq a, Typeable a ) --, Arbitrary a ) --, Coarbitrary a )



-- Packing and unpacking -------------------------------------------------------


data Somebasic where
  Somebasic :: forall a. Basic a => a -> Somebasic


pack :: forall a. Basic a => a -> Somebasic
pack = Somebasic


unpack :: forall a. Basic a => Somebasic -> Maybe a
unpack (Somebasic x)
  | Just Refl <- typeOf x ~~ r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep a


unsafeUnpack :: forall a. Basic a => Somebasic -> a
unsafeUnpack (Somebasic x)
  | Just Refl <- typeOf x ~~ r = x
  | otherwise = error $ "Data.Basic.unsafeUnpack: Types '" <> show (typeOf x) <> "' and '" <> show r <> "' did not match"
  where
    r = typeRep :: TypeRep a



-- Instances --


instance Pretty Somebasic where
  pretty (Somebasic x) = pretty x


instance Eq Somebasic where
  Somebasic x == Somebasic y
    | Just Refl <- x ~= y = x == y
    | otherwise = False



-- Primitives ------------------------------------------------------------------


class Basic a => Prim a


instance Prim Int
instance Prim Bool
instance Prim String

instance ( Prim a, Prim b ) => Prim ( a, b )
