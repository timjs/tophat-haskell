module Control.Monad.Mem
  ( module Data.Heap
  , MemT, runMemT, execMemT, evalMemT
  , Mem, runMem, execMem, evalMem
  ) where


import Preload

import Control.Monad.Ref

import Data.Basic
import Data.Heap (Heap, Loc)
import qualified Data.Heap as Heap



-- Transformer -----------------------------------------------------------------


newtype MemT m a
  = MemT (StateT Heap m a)
  deriving (Functor, Applicative, Monad)

deriving instance Monad m => MonadState Heap (MemT m)


instance Monad m => MonadRef Loc (MemT m) where

  ref :: Basic a => a -> MemT m (Loc a)
  ref x = do
    m <- get
    let ( l, m' ) = Heap.insert x m
    put m'
    pure l

  deref :: Basic a => Loc a -> MemT m a
  deref l = do
    m <- get
    pure $ Heap.lookup l m

  assign :: Basic a => Loc a -> a -> MemT m ()
  assign l x = do
    m <- get
    let m' = Heap.adjust (const x) l m
    put m'



runMemT :: MemT m a -> m ( a, Heap )
runMemT (MemT st) = runStateT st Heap.empty


evalMemT :: Monad m => MemT m a -> m a
evalMemT mem = fst <$> runMemT mem


execMemT :: Monad m => MemT m a -> m Heap
execMemT mem = snd <$> runMemT mem


type Mem = MemT Identity


runMem :: Mem a -> ( a, Heap )
runMem (MemT st) = runIdentity $ runStateT st Heap.empty


evalMem :: Mem a -> a
evalMem mem = fst $ runMem mem


execMem :: Mem a -> Heap
execMem mem = snd $ runMem mem
