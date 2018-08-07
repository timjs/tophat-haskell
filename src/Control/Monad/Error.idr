module Control.Monad.Error


import Control.Catchable


%default total
%access public export


interface ( Monad m, Catchable m e ) => MonadError e (m : Type -> Type) | m where {}
