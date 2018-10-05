module Preload
  ( module Protolude
  , module Data.Bitraversable
  , neutral
  , (<&>), unit
  ) where


import Protolude hiding ((<&>), (<&&>), trace, handle)

import Data.Bitraversable


-- Extras ----------------------------------------------------------------------


infixl 5 <&>
(<&>) :: Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = (,) <$> x <*> y


unit :: Applicative f => f ()
unit = pure ()


neutral :: Monoid m => m
neutral = mempty
