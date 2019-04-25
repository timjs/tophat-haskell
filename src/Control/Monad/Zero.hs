module Control.Monad.Zero
  ( module Control.Monad.Fail
  , MonadZero
  ) where


import Control.Monad.Fail



type MonadZero m = ( Alternative m, MonadFail m )
