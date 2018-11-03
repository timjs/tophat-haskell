module Control.Monad.Mem
  ( Heap
  , MemT, runMemT, execMemT, evalMemT
  , Mem, runMem, execMem, evalMem
  ) where


import Preload hiding (empty)

import Control.Monad.Ref

import Data.Basic
import Data.Maybe (fromJust)
import qualified Data.IntMap.Strict as IntMap



-- Heap ------------------------------------------------------------------------


data Loc a
  = Loc Int

newtype Heap
  = Heap (IntMap Somebasic)
  deriving (Show, Eq)



-- Helpers --


empty :: Heap
empty = Heap IntMap.empty


--TODO: restrict scope of Loc
insert :: forall a. Basic a => a -> Heap -> ( Loc a, Heap )
insert x (Heap vs) =
  ( l, Heap $ IntMap.insert n (pack x) vs )
  where
    n = IntMap.size vs
    l = Loc n :: Loc a


lookup :: forall a. Basic a => Loc a -> Heap -> a
lookup (Loc n) (Heap vs) =
  --XXX: unsafe things happen here...
  fromJust $ unsafeUnpack <$> IntMap.lookup n vs


adjust :: forall a. Basic a => (a -> a) -> Loc a -> Heap -> Heap
adjust f (Loc n) (Heap vs) =
  Heap $ IntMap.adjust f' n vs
  where
  --XXX: unsafe things happen here...
    f' = unsafeUnpack >> f >> pack



-- Transformer -----------------------------------------------------------------


newtype MemT m a
  = MemT (StateT Heap m a)
  deriving (Functor, Applicative, Monad)

deriving instance Monad m => MonadState Heap (MemT m)


instance Monad m => MonadRef Loc (MemT m) where

  ref :: Basic a => a -> MemT m (Loc a)
  ref x = do
    m <- get
    let ( l, m' ) = insert x m
    put m'
    pure l

  deref :: Basic a => Loc a -> MemT m a
  deref l = do
    m <- get
    pure $ lookup l m

  assign :: Basic a => Loc a -> a -> MemT m ()
  assign l x = do
    m <- get
    let m' = adjust (const x) l m
    put m'



runMemT :: MemT m a -> m ( a, Heap )
runMemT (MemT st) = runStateT st empty


evalMemT :: Monad m => MemT m a -> m a
evalMemT mem = fst <$> runMemT mem


execMemT :: Monad m => MemT m a -> m Heap
execMemT mem = snd <$> runMemT mem


type Mem = MemT Identity


runMem :: Mem a -> ( a, Heap )
runMem (MemT st) = runIdentity $ runStateT st empty


evalMem :: Mem a -> a
evalMem mem = fst $ runMem mem


execMem :: Mem a -> Heap
execMem mem = snd $ runMem mem
