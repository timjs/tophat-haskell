module Preload
  ( module Protolude
  , neutral
  , (<&>), unit
  ) where


import Protolude hiding ((<&>), (<&&>))


-- Extras ----------------------------------------------------------------------


infixl 5 <&>
(<&>) :: Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = (,) <$> x <*> y


unit :: Applicative f => f ()
unit = pure ()


neutral :: Monoid m => m
neutral = mempty
