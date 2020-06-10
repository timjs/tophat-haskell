{-# LANGUAGE TemplateHaskell #-}

-- | Based on `Polysemy.Fresh` from the `polysemy-zoo` package.
module Polysemy.Supply
  ( -- * Effect
    Supply (..),

    -- * Actions
    supply,

    -- * Interpretations
    supplyToIO,
  )
where

import Data.Unique
import Polysemy

-- | An effect for creating unique objects which may be used as references,
-- a la 'Unique'. Polymorphic code making use of 'Supply' is expected
-- to place constraints upon @s@ as necessary.
--
-- Any interpreter for 'Supply' has the responsibilty of ensuring
-- that any call to 'supply' produces an object that /never/
-- compares equal to an object produced by a previous call to 'supply'.
data Supply s m a where
  Supply :: Supply s m s

makeSem ''Supply

-- | Runs a 'Supply' effect through generating 'Unique's using 'IO'.
supplyToIO :: Member (Embed IO) r => Sem (Supply Nat ': r) a -> Sem r a
supplyToIO = interpret \Supply -> embed (newUnique ||> hashUnique ||> fromIntegral)
{-# INLINE supplyToIO #-}
