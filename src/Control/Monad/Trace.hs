module Control.Monad.Trace where


-- Class -----------------------------------------------------------------------

class ( Pretty s, Monad m ) => MonadTrace s m where
  trace :: s -> m ()


-- Instances -------------------------------------------------------------------

instance ( Pretty s ) => MonadTrace s IO where
  trace s = do
    putStrLn <| "** " <> show (pretty s)

instance ( Monoid w, Pretty s, MonadTrace s m ) => MonadTrace s (WriterT w m) where
  trace = lift << trace
