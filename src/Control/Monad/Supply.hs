-- | Support for computations which consume values from an _infinite_ supply.
-- | See <http://www.haskell.org/haskellwiki/New_monads/MonadSupply> for more details.
module Control.Monad.Supply
  ( MonadSupply (..),
  )
where

import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.Unique (hashUnique, newUnique)

-- | MonadSupply class to support getting the next item from a stream.
class Monad m => MonadSupply s m | m -> s where
  supply :: m s

instance MonadSupply Nat IO where
  supply = newUnique ||> hashUnique ||> fromIntegral

---- Monad transformer instances

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
  supply = lift supply

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply

instance (Monoid w, MonadSupply s m) => MonadSupply s (Lazy.WriterT w m) where
  supply = lift supply

instance (Monoid w, MonadSupply s m) => MonadSupply s (Strict.WriterT w m) where
  supply = lift supply
