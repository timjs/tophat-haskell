module Control.Monad.Ref
  ( MonadRef(..), (<<-), (<<=)
  ) where


class ( Monad m ) => MonadRef l m | m -> l where
  ref    :: a -> m (l a)
  deref  :: l a -> m a
  assign :: l a -> a -> m ()

infixl 1 <<-
infixl 1 <<=

(<<-) :: MonadRef l m => l a -> a -> m ()
(<<-) = assign

(<<=) :: MonadRef l m => l a -> (a -> a) -> m ()
(<<=) l f = do
  x <- deref l
  assign l (f x)

-- instance ( MonadRef l m, Monoid w ) => MonadRef l (WriterT w m) where
--   ref    = lift << ref
--   assign l = lift << assign l
--   deref    = lift << deref
--
-- instance ( MonadRef l m ) => MonadRef l (ExceptT e m) where
--   ref    = lift << ref
--   assign l = lift << assign l
--   deref    = lift << deref
--
-- instance MonadRef IORef IO where
--   ref  = newIORef
--   assign = writeIORef
--   deref  = readIORef
--
