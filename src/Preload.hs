module Preload
  ( module Protolude
  , module Data.Bitraversable
  , List
  , Pretty(..)
  , neutral
  , (<&>), unit
  , forany, forall
  , sameT
  ) where


import Protolude hiding ((<&>), (<&&>), trace, handle)

import Data.Bitraversable



-- Synonyms --------------------------------------------------------------------


type List a = [a]



-- Monoids ---------------------------------------------------------------------


{-# INLINE neutral #-}
neutral :: Monoid m => m
neutral = mempty



-- Applicatives ----------------------------------------------------------------


infixl 5 <&>
(<&>) :: Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = (,) <$> x <*> y


{-# INLINE unit #-}
unit :: Applicative f => f ()
unit = pure ()



-- Monads ----------------------------------------------------------------------


-- | A version of 'any' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > forany Just [False,True ,undefined] == Just True
-- > forany Just [False,False,undefined] == undefined
-- > \(f :: Int -> Maybe Bool) xs -> forany f xs == orM (map f xs)
forany :: Monad m => List a -> (a -> m Bool) -> m Bool
forany [] _ = return False
forany (x:xs) p = ifM (p x) (return True) (forany xs p)


-- | A version of 'all' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > forall Just [True,False,undefined] == Just False
-- > forall Just [True,True ,undefined] == undefined
-- > \(f :: Int -> Maybe Bool) xs -> forany f xs == orM (map f xs)
forall :: Monad m => List a -> (a -> m Bool) -> m Bool
forall [] _ = return True
forall (x:xs) p = ifM (p x) (forall xs p) (return False)



-- Type equality ---------------------------------------------------------------


{-# INLINE sameT #-}
sameT :: ( Typeable a, Typeable b ) => a -> b -> Maybe (a :~: b)
sameT _ _ = eqT



-- Pretty printing -------------------------------------------------------------


class Pretty a where
  pretty :: a -> Text
