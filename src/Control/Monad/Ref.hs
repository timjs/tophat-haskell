module Control.Monad.Ref
  ( MonadRef(..), (<<-), (<<=)
  ) where


class ( Monad m ) => MonadRef l m | m -> l where
  new   :: a -> m (l a)
  read  :: l a -> m a
  write :: l a -> a -> m ()

infixl 1 <<-
infixl 1 <<=

(<<-) :: MonadRef l m => l a -> a -> m ()
(<<-) = write

(<<=) :: MonadRef l m => l a -> (a -> a) -> m ()
(<<=) l f = do
  x <- read l
  write l (f x)

-- instance ( MonadRef l m, Monoid w ) => MonadRef l (WriterT w m) where
--   new    = lift << new
--   write l = lift << write l
--   read    = lift << read
--
-- instance ( MonadRef l m ) => MonadRef l (ExceptT e m) where
--   new    = lift << new
--   write l = lift << write l
--   read    = lift << read

instance MonadRef IORef IO where
  new    = newIORef
  read  = readIORef
  write = writeIORef
