module Control.Monad.Trace


%default total
%access public export



-- Interface -------------------------------------------------------------------


interface ( Show l, Monad m ) => MonadTrace l (m : Type -> Type) | m where
  trace : l -> a -> m a



-- Instances -------------------------------------------------------------------


Show l => MonadTrace l IO where
  trace l x = do
    putStrLn $ "** " ++ show l
    pure x
