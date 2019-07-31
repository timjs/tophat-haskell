module Control.Interactive where

import Data.Editable
import Data.HashMap.Strict (lookup)


class Monad m => Interactive m where
  enter :: Editable a => m a

  update :: Editable a => a -> m a

  view :: Editable a => a -> m a
  view = update

  pick :: Dict Label (m a) -> m a
  pick ms = do
    l <- enter
    case lookup l ms of
      Just m -> m
      Nothing -> pick ms


type Label = Text
