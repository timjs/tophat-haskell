module Control.Monad.Log where


-- Class -----------------------------------------------------------------------

class ( Pretty a, Monad m ) => MonadLog a m where
  log :: Severity -> a -> m ()


instance ( Pretty a ) => MonadLog a IO where
  log s a = do
    putStr <| show <| pretty s
    putStr " "
    putStrLn <| show <| pretty a

instance ( Monoid w, Pretty s, MonadLog s m ) => MonadLog s (WriterT w m) where
  log s = lift << log s


-- Severity --------------------------------------------------------------------

data Severity
  = Error
  | Warning
  | Info

instance Pretty Severity where
  pretty = \case
    Error -> "!!"
    Warning -> "**"
    Info -> "=="
