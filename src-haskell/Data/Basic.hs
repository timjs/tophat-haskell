module Data.Basic
  ( Basic
  ) where


import Preload

import Test.QuickCheck (Arbitrary)



-- Class -----------------------------------------------------------------------


type Basic a = ( Show a, Eq a, Typeable a, Arbitrary a ) -- Coarbitrary a )
