module Data.Store
  ( -- * Data
    Store (..),

    -- * Operations
    alloc,
    read,
    write,
    focus,
    _identity,

    -- * Inspecting
    Inspectable,
  )
where

import Data.Heap (Ref)
import Lens.Simple (Lens', iso, set, view)
import Polysemy
import Polysemy.Mutate (Alloc, Read, Write)
import qualified Polysemy.Mutate as Mutate

-- Stores ----------------------------------------------------------------------

-- NOTE:
-- I think the source and target types always have to be the same,
-- otherwise you would be able to change the type of the inner reference...
-- Isn't it?
-- NOTE: `s` should be `Typeable` to store it as `Someref`.
data Store h a = forall s. (Inspectable h s) => Store (Lens' s a) (Ref h s)

alloc :: (Member (Alloc h) r, Inspectable h a) => a -> Sem r (Store h a)
alloc x = do
  r <- Mutate.alloc x
  pure <| Store _identity r

read :: (Member (Read h) r) => Store h a -> Sem r a
read (Store l r) = do
  s <- Mutate.read r
  pure <| view l s

write :: (Members '[Read h, Write h] r) => a -> Store h a -> Sem r ()
write x (Store l r) = do
  Mutate.mutate (set l x) r

-- Focussing -------------------------------------------------------------------

_identity :: Lens' a a
_identity = iso identity identity

focus :: Lens' a b -> Store r a -> Store r b
focus l' (Store l r) = Store (l << l') r

-- Inspecting ------------------------------------------------------------------

class (Typeable (Ref h a), Eq (Ref h a)) => Inspectable h a
