{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Mutate
  ( -- * Effects
    Read (..),
    Write (..),
    Alloc (..),

    -- * Actions
    read,
    write,
    alloc,

    -- * Interpretations
    readToIO,
    writeToIO,
    allocToIO,
  )
where

import Polysemy

-- Effects ---------------------------------------------------------------------

data Read l m a where
  Read :: l a -> Read l m a

data Write l m a where
  Write :: a -> l a -> Write l m ()

data Alloc l m a where
  Alloc :: a -> Alloc l m (l a)

makeSem ''Read
makeSem ''Write
makeSem ''Alloc

-- Interpretations -------------------------------------------------------------

readToIO :: Member (Embed IO) r => Sem (Read IORef ': r) a -> Sem r a
readToIO = interpret \case
  Read loc -> readIORef loc

writeToIO :: Member (Embed IO) r => Sem (Write IORef ': r) a -> Sem r a
writeToIO = interpret \case
  Write val loc -> writeIORef loc val

allocToIO :: Member (Embed IO) r => Sem (Alloc IORef ': r) a -> Sem r a
allocToIO = interpret \case
  Alloc val -> newIORef val
