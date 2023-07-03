{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Abort
  ( -- * Effect
    Abort,

    -- * Actions
    abort,

    -- * Interpretations

    -- runAbort,
    abortToError,
    abortToNonDet,
    abortToIO,
  )
where

import Polysemy
import Polysemy.Error
import Polysemy.NonDet

---- Effect --------------------------------------------------------------------

data Abort m a where
  Abort :: Abort m a

makeSem ''Abort

---- Interpretations -----------------------------------------------------------

-- -- | Run a 'Abort' effect purely.
-- runAbort ::
--   a ->
--   Sem (Abort ': r) a ->
--   Sem r a
-- runAbort a = interpret \case
--   Abort -> pure Nothing
-- {-# INLINE runAbort #-}

------------------------------------------------------------------------------

-- | Transform a 'Abort' effect into an @'Error' e@ effect,
-- through providing a function for transforming any aborture
-- to an exception.
abortToError ::
  (Member (Error e) r) =>
  e ->
  Sem (Abort ': r) a ->
  Sem r a
abortToError x = interpret \case
  Abort -> throw x
{-# INLINE abortToError #-}

------------------------------------------------------------------------------

-- | Transform a 'Abort' effect into a 'NonDet' effect,
-- through mapping any aborture to 'empty'.
abortToNonDet ::
  (Member NonDet r) =>
  Sem (Abort ': r) a ->
  Sem r a
abortToNonDet = interpret \case
  Abort -> empty
{-# INLINE abortToNonDet #-}

------------------------------------------------------------------------------

-- | Run a 'Abort' effect in terms of the underlying 'IO' instance.
abortToIO ::
  (Member (Embed IO) r) =>
  Sem (Abort ': r) a ->
  Sem r a
abortToIO = interpret \case
  Abort -> embed exitSuccess
{-# INLINE abortToIO #-}
