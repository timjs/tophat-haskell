module Data.Editable
  ( Editable, Storable
  ) where


type Editable a = ( Pretty a, Read a, Eq a, Typeable a ) --, Arbitrary a ) --, Coarbitrary a )


class Editable a => Storable a

instance Storable Int
instance Storable Bool
instance Storable String
instance ( Storable a, Storable b ) => Storable ( a, b )
