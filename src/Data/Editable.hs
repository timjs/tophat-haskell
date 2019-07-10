module Data.Editable
  ( Editable
  ) where


type Editable a = ( Pretty a, Read a, Eq a, Typeable a ) --, Arbitrary a ) --, Coarbitrary a )
