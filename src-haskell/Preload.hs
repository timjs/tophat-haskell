module Preload
  ( module Protolude
  , module Control.Monad.Zero
  , module Data.Bitraversable
  , words, unwords, lines, unlines
  , List
  , (<<), (>>)
  , Pretty(..), module GHC.Show, showText, read
  , neutral
  , ok, throw, catch
  , (<&>), skip
  , forany, forall
  , sameT, proxyOf, arbitraryOf, typeOf
  ) where


import Protolude hiding ((<&>), (<&&>), (.), (>>), trace, handle, lift, TypeRep, typeRep, catch)

import Control.Monad.Zero

import Data.Bitraversable
import Data.Text (unpack, words, unwords, lines, unlines)

import Test.QuickCheck (Arbitrary(..), Gen)

import Type.Reflection (typeOf)

import GHC.Show (Show(showsPrec), ShowS, showString, showParen)



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



-- MonadError ------------------------------------------------------------------


{-# INLINE ok #-}
ok :: MonadError e m => a -> m a
ok = pure


{-# INLINE throw #-}
throw :: MonadError e m => e -> m a
throw = throwError


{-# INLINE catch #-}
catch :: MonadError e m => m a -> (e -> m a) -> m a
catch = catchError



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



-- Reading & Showing -----------------------------------------------------------


class Pretty a where
  pretty :: a -> Text


showText :: Text -> ShowS
showText = showString << toS


read :: Read a => Text -> Maybe a
read = readMaybe << unpack
