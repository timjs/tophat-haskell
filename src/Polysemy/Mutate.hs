{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Mutate
  ( -- * Effects
    Read (..),
    Write (..),
    Alloc (..),
    Mutate,

    -- ** Heaps
    Heap (..),
    Ref,

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

-- Heap ------------------------------------------------------------------------

data Heap h
  = Global
  | Local h

type family Ref h where
  Ref ('Global) = IORef
  Ref ('Local h') = STRef h'

-- Effects ---------------------------------------------------------------------

data Read h m a where
  Read :: Ref h a -> Read h m a

data Write h m a where
  Write :: a -> Ref h a -> Write h m ()

data Alloc h m a where
  Alloc :: a -> Alloc h m (Ref h a)

makeSem ''Read
makeSem ''Write
makeSem ''Alloc

-- Operators -------------------------------------------------------------------

mutate :: Members '[Read h, Write h] r => (a -> a) -> Ref h a -> Sem r ()
mutate f r = do
  x <- read r
  write (f x) r

infixl 1 <<-

infixl 1 <<=

(<<-) :: Members '[Read h, Write h] r => Ref h a -> a -> Sem r ()
(<<-) = flip write

(<<=) :: Members '[Read h, Write h] r => Ref h a -> (a -> a) -> Sem r ()
(<<=) = flip mutate

-- Bundle ----------------------------------------------------------------------

type Mutate h = Bundle '[Read h, Write h, Alloc h]

read' :: (Member (Read h) r', Member (Bundle r') r) => Ref h a -> Sem r a
read' = sendBundle << read

write' :: (Member (Write h) r', Member (Bundle r') r) => a -> Ref h a -> Sem r ()
write' val = sendBundle << write val

alloc' :: (Member (Alloc h) r', Member (Bundle r') r) => a -> Sem r (Ref h a)
alloc' = sendBundle << alloc

-- mutate' :: (Members '[Read h, Write h] r', Member (Bundle r') r) => (a -> a) -> Ref h a -> Sem r ()
-- mutate' f = sendBundle << mutate f

-- Interpretations -------------------------------------------------------------

-- In IO --

readToIO :: Member (Embed IO) r => Sem (Read 'Global ': r) a -> Sem r a
readToIO = interpret \case
  Read loc -> embed <| readIORef loc

writeToIO :: Member (Embed IO) r => Sem (Write 'Global ': r) a -> Sem r a
writeToIO = interpret \case
  Write val loc -> embed <| writeIORef loc val

allocToIO :: Member (Embed IO) r => Sem (Alloc 'Global ': r) a -> Sem r a
allocToIO = interpret \case
  Alloc val -> embed <| newIORef val

-- In ST --

readToST :: Member (Embed (ST h)) r => Sem (Read ('Local h) ': r) a -> Sem r a
readToST = interpret \case
  Read loc -> embed <| readSTRef loc

writeToST :: Member (Embed (ST h)) r => Sem (Write ('Local h) ': r) a -> Sem r a
writeToST = interpret \case
  Write val loc -> embed <| writeSTRef loc val

allocToST :: Member (Embed (ST h)) r => Sem (Alloc ('Local h) ': r) a -> Sem r a
allocToST = interpret \case
  Alloc val -> embed <| newSTRef val
