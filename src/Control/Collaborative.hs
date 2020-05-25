module Control.Collaborative
  ( -- * Classes
    Collaborative (..),

    -- * Operators
    (<<-),
    (<<=),

    -- * Reexports
    module Data.Store,
    module Data.Someref,
  )
where

import Data.Edit
import Data.Someref
import Data.Store
import qualified Lens.Simple as Lens

-- Class -----------------------------------------------------------------------

-- | A variation on a monad `m` with references `r`.
-- |
-- | References `r` should be typeable, so they can be existentially stored.
-- | Also, they should be equatable, so we can compare the pointers to check if they are the same.
-- | Data `a` stored in such references should be editable,
-- | so they can be shown in an editor on screen and edited by an end user.
-- |
-- | NOTE: GHC does not let us write this as a type synonym,
-- | because of the impredicativity of `Eq (r a)`.
class (Monad m) => Collaborative h m | m -> h where
  share :: (Edit a, Inspect h a) => a -> m (Store h a)
  watch :: (Edit a) => Store h a -> m a
  assign :: (Edit a) => a -> Store h a -> m ()

  change :: (Edit a) => Store h a -> m a
  change = watch

  mutate :: (Edit a) => (a -> a) -> Store h a -> m ()
  mutate f r = do
    x <- watch r
    assign (f x) r

-- Operators -------------------------------------------------------------------

infixl 1 <<-

infixl 1 <<=

(<<-) :: (Collaborative h m, Edit a) => Store h a -> a -> m ()
(<<-) = flip assign

(<<=) :: (Collaborative h m, Edit a) => Store h a -> (a -> a) -> m ()
(<<=) = flip mutate

-- Instances -------------------------------------------------------------------

instance Collaborative 'Global IO where
  share x = do
    r <- newIORef x
    pure <| Store _identity r

  assign x (Store l r) = do
    modifyIORef r (Lens.set l x)

  watch (Store l r) = do
    s <- readIORef r
    pure <| Lens.view l s
