{-# LANGUAGE TemplateHaskell #-}

-- | Module providing reading, writing and allocating references on a heap `h`.
-- |
-- | **Important**!
-- | Because the `m` parameter will be instantiated with `Sem r`,
-- | we won't have access to the concrete monad in which we will interpret these effects.
-- | We need to know this, because we need to link the type of references used, to the monad we will interpret the references in.
-- | However, we'd like to abstract away from the concrete interpretation monad, which is the whole point of using these tricks!
-- |
-- | To solve this, we parametrise all effects over a fictional heap `h`.
-- | This way, we can link all pieces together.
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

import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Polysemy
import Polysemy.Bundle

---- Heap ----------------------------------------------------------------------

---- Effects -------------------------------------------------------------------

data Read h m a where
  Read :: STRef h a -> Read h m a

data Write h m a where
  Write :: a -> STRef h a -> Write h m ()

data Alloc h m a where
  Alloc :: a -> Alloc h m (STRef h a)

makeSem ''Read
makeSem ''Write
makeSem ''Alloc

---- Operators -----------------------------------------------------------------

mutate :: (Members '[Read h, Write h] r) => (a -> a) -> STRef h a -> Sem r ()
mutate f r = do
  x <- read r
  write (f x) r

infixl 1 <<-

infixl 1 <<=

(<<-) :: (Members '[Read h, Write h] r) => STRef h a -> a -> Sem r ()
(<<-) = flip write

(<<=) :: (Members '[Read h, Write h] r) => STRef h a -> (a -> a) -> Sem r ()
(<<=) = flip mutate

---- Bundle --------------------------------------------------------------------

type Mutate h = Bundle '[Read h, Write h, Alloc h]

read' :: (Member (Read h) r', Member (Bundle r') r) => STRef h a -> Sem r a
read' = sendBundle << read

write' :: (Member (Write h) r', Member (Bundle r') r) => a -> STRef h a -> Sem r ()
write' val = sendBundle << write val

alloc' :: (Member (Alloc h) r', Member (Bundle r') r) => a -> Sem r (STRef h a)
alloc' = sendBundle << alloc

-- mutate' :: (Members '[Read h, Write h] r', Member (Bundle r') r) => (a -> a) -> STRef h a -> Sem r ()
-- mutate' f = sendBundle << mutate f

---- Interpretations -----------------------------------------------------------

---- In IO

readToIO :: (Member (Embed IO) r) => Sem (Read RealWorld ': r) a -> Sem r a
readToIO = interpret \case
  Read loc -> embed <| stToIO <| readSTRef loc

writeToIO :: (Member (Embed IO) r) => Sem (Write RealWorld ': r) a -> Sem r a
writeToIO = interpret \case
  Write val loc -> embed <| stToIO <| writeSTRef loc val

allocToIO :: (Member (Embed IO) r) => Sem (Alloc RealWorld ': r) a -> Sem r a
allocToIO = interpret \case
  Alloc val -> embed <| stToIO <| newSTRef val

---- In ST

readToST :: (Member (Embed (ST h)) r) => Sem (Read h ': r) a -> Sem r a
readToST = interpret \case
  Read loc -> embed <| readSTRef loc

writeToST :: (Member (Embed (ST h)) r) => Sem (Write h ': r) a -> Sem r a
writeToST = interpret \case
  Write val loc -> embed <| writeSTRef loc val

allocToST :: (Member (Embed (ST h)) r) => Sem (Alloc h ': r) a -> Sem r a
allocToST = interpret \case
  Alloc val -> embed <| newSTRef val
