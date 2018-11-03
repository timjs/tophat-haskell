module Preload
  ( module Protolude
  , module Data.Bitraversable
  , List
  , (<<), (>>)
  , Pretty(..)
  , neutral
  , (<&>), skip
  , forany, forall
  , sameT, proxyOf, arbitraryOf
  ) where


import Protolude hiding ((<&>), (<&&>), (.), (>>), trace, handle, lift, TypeRep, typeRep)

import Data.Bitraversable

import Test.QuickCheck (Arbitrary(..), Gen)



-- Synonyms --------------------------------------------------------------------


type List a = [a]


-- Functions -------------------------------------------------------------------


infixr 9 <<
{-# INLINE (<<) #-}
(<<) :: (b -> c) -> (a -> b) -> a -> c
f << g = \x -> f (g x)


infixr 9 >>
{-# INLINE (>>) #-}
(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (<<)



-- Monoids ---------------------------------------------------------------------


{-# INLINE neutral #-}
neutral :: Monoid m => m
neutral = mempty



-- Applicatives ----------------------------------------------------------------


infixl 5 <&>
(<&>) :: Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = (,) <$> x <*> y


{-# INLINE skip #-}
skip :: Applicative f => f ()
skip = pure ()



-- Monads ----------------------------------------------------------------------


-- | A version of 'any' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > forany [False,True ,undefined] Just == Just True
-- > forany [False,False,undefined] Just == undefined
-- > xs \(f :: Int -> Maybe Bool) -> forany xs f == orM (map f xs)
forany :: Monad m => List a -> (a -> m Bool) -> m Bool
forany [] _ = return False
forany (x:xs) p = ifM (p x) (return True) (forany xs p)


-- | A version of 'all' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > forall [True,False,undefined] Just == Just False
-- > forall [True,True ,undefined] Just == undefined
-- > xs \(f :: Int -> Maybe Bool) -> forany xs f == orM (map f xs)
forall :: Monad m => List a -> (a -> m Bool) -> m Bool
forall [] _ = return True
forall (x:xs) p = ifM (p x) (forall xs p) (return False)



-- Type equality & Proxys ------------------------------------------------------


{-# INLINE sameT #-}
sameT :: ( Typeable a, Typeable b ) => a -> b -> Maybe (a :~: b)
sameT _ _ = eqT


{-# INLINE proxyOf #-}
proxyOf :: a -> Proxy a
proxyOf _ = Proxy


{-# INLINE arbitraryOf #-}
arbitraryOf :: Arbitrary a => Proxy a -> Gen a
arbitraryOf _ = arbitrary



-- Pretty printing -------------------------------------------------------------


class Pretty a where
  pretty :: a -> Text
