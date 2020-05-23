module Data.Store
  ( Store (..),
    alloc,
    read,
    write,
    focus,
    _identity,
  )
where

import Lens.Simple (Lens', iso, set, view)
import Polysemy
import Polysemy.Mutate (Alloc, Read, Ref, Write)
import qualified Polysemy.Mutate as Mutate

-- Stores ----------------------------------------------------------------------

-- NOTE:
-- I think the source and target types always have to be the same,
-- otherwise you would be able to change the type of the inner reference...
-- Isn't it?
data Store h a = forall s. Typeable s => Store (Lens' s a) (Ref h s)

alloc :: (Member (Alloc h) r, Typeable a) => a -> Sem r (Store h a)
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
