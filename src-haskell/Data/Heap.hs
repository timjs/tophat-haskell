module Data.Heap
  ( module Data.Basic
  , Heap, Loc
  , empty
  , insert
  , lookup
  , adjust
  ) where


import Preload hiding (empty)

import Data.Basic
import Data.Maybe (fromJust)
import qualified Data.IntMap.Strict as IntMap



-- Heap ------------------------------------------------------------------------


data Loc a
  = Loc Int

newtype Heap
  = Heap (IntMap Somebasic)
  deriving (Show, Eq)



-- Creation --------------------------------------------------------------------


empty :: Heap
empty = Heap IntMap.empty



-- Manipulation ----------------------------------------------------------------


--TODO: restrict scope of Loc
insert :: forall a. Basic a => a -> Heap -> ( Loc a, Heap )
insert x (Heap vs) =
  ( l, Heap $ IntMap.insert n (pack x) vs )
  where
    n = IntMap.size vs
    l = Loc n :: Loc a


lookup :: forall a. Basic a => Loc a -> Heap -> a
lookup (Loc n) (Heap vs) =
  --XXX: unsafe things happen here...
  fromJust $ unsafeUnpack <$> IntMap.lookup n vs


adjust :: forall a. Basic a => (a -> a) -> Loc a -> Heap -> Heap
adjust f (Loc n) (Heap vs) =
  Heap $ IntMap.adjust f' n vs
  where
  --XXX: unsafe things happen here...
    f' = unsafeUnpack >> f >> pack
