module Control.Interactive where

import Data.Editable
import qualified Data.HashMap.Strict as Dict


class ( Monad m ) => Interactive m where
  enter :: Editable a => m a

  update :: Editable a => a -> m a

  view :: Editable a => a -> m a
  view = update

  pick :: Dict Label (m a) -> m a
  pick ms = do
    _ <- view <| Dict.keys ms
    l <- enter
    case Dict.lookup l ms of
      Just m -> m
      Nothing -> pick ms

type Label = Text

instance ( Interactive m, Monoid w ) => Interactive (WriterT w m) where
  enter = lift enter
  update = lift << update
  view = lift << view
  -- pick = lift << pick

instance ( Interactive m ) => Interactive (ExceptT e m) where
  enter = lift enter
  update = lift << update
  view = lift << view
  -- pick = lift << pick

instance Interactive IO where

  enter :: forall a. Editable a => IO a
  enter = do
    putText <| "Enter a " <> show tau <> ":"
    t <- getLine
    case scan t of
      Just x -> pure x
      Nothing -> enter
    where
      tau = typeRep :: TypeRep a

  update :: forall a. Editable a => a -> IO a
  update x = do
    putText <| "Update " <> show (pretty x) <> ":"
    t <- getLine
    case scan t of
      Just x' -> pure x'
      Nothing -> update x

  view :: forall a. Editable a => a -> IO a
  view x = do
    putText <| "View:" <> show (pretty x)
    pure x
