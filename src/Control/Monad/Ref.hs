module Control.Monad.Ref
  ( MonadRef(..), (<<-), (<<=)
  ) where


class ( Monad m ) => MonadRef r m | m -> r where
  new   :: a -> m (r a)
  read  :: r a -> m a
  write :: r a -> a -> m ()

infixl 1 <<-
infixl 1 <<=

(<<-) :: MonadRef r m => r a -> a -> m ()
(<<-) = write

(<<=) :: MonadRef r m => r a -> (a -> a) -> m ()
(<<=) r f = do
  x <- read r
  write r (f x)

-- instance ( MonadRef r m, Monoid w ) => MonadRef r (WriterT w m) where
--   new    = lift << new
--   write r = lift << write r
--   read    = lift << read
--
-- instance ( MonadRef r m ) => MonadRef r (ExceptT e m) where
--   new    = lift << new
--   write r = lift << write r
--   read    = lift << read

instance MonadRef IORef IO where
  new   = newIORef
  read  = readIORef
  write = writeIORef
