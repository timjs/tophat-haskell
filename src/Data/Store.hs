module Data.Store
  ( -- * Data
    Store (..),
    Ref,
    RealWorld,

    -- * Operations
    alloc,
    read,
    write,
    focus,
    iso,
    _identity,
  )
where

import Control.Monad.ST (RealWorld)
import Data.STRef (STRef)
import Lens.Family (LensLike, LensLike', set, under, view)
import Lens.Family.Unchecked (adapter)
import Polysemy
import Polysemy.Mutate (Alloc, Read, Write)
import qualified Polysemy.Mutate as Mutate

---- Stores --------------------------------------------------------------------

type Ref = STRef

-- | **Note**
-- | I think the source and target types always have to be the same,
-- | otherwise you would be able to change the type of the inner reference...
-- | Isn't it?
-- |
-- | **Note**
-- | `s` should be `Reflect` to store it as `Someref`.
-- |
-- | **Important!**
-- | When using Polysemy, the `Read`, `Write`, and `Alloc` effects range over a heap `h`.
-- | So we have only access to this `h` parameter.
-- | We need to have a way to link the heap which the effects operate on,
-- | to the type of references we save in a `Store`.
-- | This is why stores are parametrised over a heap `h`,
-- | and not over a monad `m`.
-- |
-- | See for example `alloc`, which needs `Alloc h` as a member, and returns a `Sem r (Store h a)`.
-- | The `h` parameter links the reference type of the effect (`Alloc`), to the reference type of the `Store`.
-- | In past days, we used a type class with fundep, which deduced the reference type from the monad `m`.
-- | But now we don't know in which (final) `m` we're going to interpret `Sem r`!
data Store h a where
  Store {- exists s -} :: (Reflect h, Reflect s) => Lens' s a -> Ref h s -> Store h a

alloc :: (Member (Alloc h) r, Reflect h, Reflect a) => a -> Sem r (Store h a)
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

---- Focussing -----------------------------------------------------------------

type Lens' s a = forall f. (Functor f) => LensLike' f s a

iso :: (Functor f) => (s -> a) -> (b -> t) -> LensLike f s t a b
iso f g = under <| adapter f g

_identity :: Lens' a a
_identity = iso identity identity

focus :: Lens' a b -> Store r a -> Store r b
focus l' (Store l r) = Store (l << l') r
