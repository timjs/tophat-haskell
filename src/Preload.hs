module Preload
  ( module Protolude
  , module Data.Bitraversable
  , Pretty(..)
  , neutral
  , (<&>), unit
  , sameT
  ) where


import Protolude hiding ((<&>), (<&&>), trace, handle)

import Data.Bitraversable



-- Extras ----------------------------------------------------------------------


infixl 5 <&>
(<&>) :: Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = (,) <$> x <*> y


{-# INLINE unit #-}
unit :: Applicative f => f ()
unit = pure ()


{-# INLINE neutral #-}
neutral :: Monoid m => m
neutral = mempty



-- Type equality ---------------------------------------------------------------


{-# INLINE sameT #-}
sameT :: ( Typeable a, Typeable b ) => a -> b -> Maybe (a :~: b)
sameT _ _ = eqT



-- Pretty printing -------------------------------------------------------------


class Pretty a where
  pretty :: a -> Text
