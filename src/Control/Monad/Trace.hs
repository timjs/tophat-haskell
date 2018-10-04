module Control.Monad.Trace where


import Base



-- Class -----------------------------------------------------------------------


class ( Show s, Monad m ) => MonadTrace s m where
  trace :: s -> a -> m a



-- Instances -------------------------------------------------------------------


instance Show s => MonadTrace s IO where
  trace s x = do
    putStrLn $ "** " ++ show s
    pure x
