module Control.Interactive where


import Data.Editable


class Functor f => Interactive f where
  enter :: Editable a => f a

  update :: Editable a => a -> f a

  view :: Editable a => a -> f a
  view = update
