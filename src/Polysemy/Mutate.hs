{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Mutate
  ( -- * Effects
    Read (..),
    Write (..),
    Alloc (..),
    Mutate,

    -- * Actions
    read,
    read',
    write,
    write',
    alloc,
    alloc',
    mutate,
    -- mutate',
    (<<-),
    (<<=),

    -- * Interpretations
    readToIO,
    writeToIO,
    allocToIO,
    readToST,
    writeToST,
    allocToST,
  )
where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Polysemy
import Polysemy.Bundle

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

-- Operators -------------------------------------------------------------------

mutate :: Members '[Read l, Write l] r => (a -> a) -> l a -> Sem r ()
mutate f r = do
  x <- read r
  write (f x) r

infixl 1 <<-

infixl 1 <<=

(<<-) :: Members '[Read l, Write l] r => l a -> a -> Sem r ()
(<<-) = flip write

(<<=) :: Members '[Read l, Write l] r => l a -> (a -> a) -> Sem r ()
(<<=) = flip mutate

-- Bundle ----------------------------------------------------------------------

type Mutate l = Bundle '[Read l, Write l, Alloc l]

read' :: (Member (Read l) r', Member (Bundle r') r) => l a -> Sem r a
read' = sendBundle << read

write' :: (Member (Write l) r', Member (Bundle r') r) => a -> l a -> Sem r ()
write' val = sendBundle << write val

alloc' :: (Member (Alloc l) r', Member (Bundle r') r) => a -> Sem r (l a)
alloc' = sendBundle << alloc

-- mutate' :: (Members '[Read l, Write l] r', Member (Bundle r') r) => (a -> a) -> l a -> Sem r ()
-- mutate' f = sendBundle << mutate f

-- Interpretations -------------------------------------------------------------

-- In IO --

readToIO :: Member (Embed IO) r => Sem (Read IORef ': r) a -> Sem r a
readToIO = interpret \case
  Read loc -> embed <| readIORef loc

writeToIO :: Member (Embed IO) r => Sem (Write IORef ': r) a -> Sem r a
writeToIO = interpret \case
  Write val loc -> embed <| writeIORef loc val

allocToIO :: Member (Embed IO) r => Sem (Alloc IORef ': r) a -> Sem r a
allocToIO = interpret \case
  Alloc val -> embed <| newIORef val

-- In ST --

readToST :: Member (Embed (ST h)) r => Sem (Read (STRef h) ': r) a -> Sem r a
readToST = interpret \case
  Read loc -> embed <| readSTRef loc

writeToST :: Member (Embed (ST h)) r => Sem (Write (STRef h) ': r) a -> Sem r a
writeToST = interpret \case
  Write val loc -> embed <| writeSTRef loc val

allocToST :: Member (Embed (ST h)) r => Sem (Alloc (STRef h) ': r) a -> Sem r a
allocToST = interpret \case
  Alloc val -> embed <| newSTRef val
