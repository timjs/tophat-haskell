module Control.Monad.Ref
  ( MonadRef (..),
    (<<-),
    (<<=),
  )
where

-- Class -----------------------------------------------------------------------

class (Monad m) => MonadRef r m | m -> r where
  new :: a -> m (r a)
  read :: r a -> m a
  write :: a -> r a -> m ()

  mutate :: (a -> a) -> r a -> m ()
  mutate f r = do
    x <- read r
    r <<- (f x)

-- Operators -------------------------------------------------------------------

infixl 1 <<-

infixl 1 <<=

(<<-) :: MonadRef r m => r a -> a -> m ()
(<<-) = flip write

(<<=) :: MonadRef r m => r a -> (a -> a) -> m ()
(<<=) = flip mutate

-- Instances -------------------------------------------------------------------

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
  new = newIORef
  read = readIORef
  write = flip writeIORef
