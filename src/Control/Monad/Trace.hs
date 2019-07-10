module Control.Monad.Trace where



-- Class -----------------------------------------------------------------------


class ( Pretty s, Monad m ) => MonadTrace s m where
  trace :: s -> m a -> m a



-- Instances -------------------------------------------------------------------


instance Pretty s => MonadTrace s IO where
  trace s x = do
    putStrLn <| "** " <> show (pretty s)
    x
