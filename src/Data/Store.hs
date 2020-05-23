module Data.Store
  ( Store (..),
    ref,
    deref,
    assign,
    focus,
    _identity,
  )
where

import Lens.Simple (Lens', iso, set, view)
import Polysemy
import Polysemy.Mutate

-- Stores ----------------------------------------------------------------------

-- NOTE:
-- I think the source and target types always have to be the same,
-- otherwise you would be able to change the type of the inner reference...
-- Isn't it?
data Store h a = forall s. Typeable s => Store (Lens' s a) (Ref h s)

ref :: (Member (Alloc h) r, Typeable a) => a -> Sem r (Store h a)
ref x = do
  r <- alloc x
  pure <| Store _identity r

deref :: (Member (Read h) r) => Store h a -> Sem r a
deref (Store l r) = do
  s <- read r
  pure <| view l s

assign :: (Members '[Read h, Write h] r) => a -> Store h a -> Sem r ()
assign x (Store l r) = do
  mutate (set l x) r

-- Focussing -------------------------------------------------------------------

_identity :: Lens' a a
_identity = iso identity identity

focus :: Lens' a b -> Store r a -> Store r b
focus l' (Store l r) = Store (l << l') r
