module Data.Heap
  ( Heap (..),
    Ref,
  )
where

import Data.STRef (STRef)

data Heap h
  = Global
  | Local h

type family Ref h where
  Ref ('Global) = IORef
  Ref ('Local h') = STRef h'
