module Data.Basic
  ( Basic
  ) where


import Protolude



-- Class -----------------------------------------------------------------------


type Basic a = ( Show a, Eq a ) -- Arbitrary a, Coarbitrary a )
