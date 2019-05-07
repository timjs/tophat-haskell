module Control.Interactive where


import Data.Editable


class Applicative f => Interactive f where
  enter :: Editable a => f a
  edit :: Editable a => a -> f a

  view :: Editable a => a -> f a
  view = edit
