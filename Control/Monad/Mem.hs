module Control.Monad.Mem
  ( Memory, Heap, Locs
  , MemT, runMemT, execMemT, evalMemT
  , Mem, runMem, execMem, evalMem
  ) where


import Preload hiding (empty)

import Control.Monad.Ref

import Data.Maybe (fromJust)
import qualified Data.IntMap.Strict as IntMap

import Unsafe.Coerce (unsafeCoerce)



-- Cells -----------------------------------------------------------------------


data Cell :: Type where
  Pack :: a -> Cell


pack :: a -> Cell
pack = Pack


unpack :: Cell -> a
--XXX: unsafe things happen here...
unpack (Pack x) = unsafeCoerce x



-- Heap ------------------------------------------------------------------------


data Memory
  = Memory Locs Heap

type Heap = IntMap Cell
type Locs = List Cell

data Loc a = Loc Int



-- Helpers --


empty :: Memory
empty = Memory [] IntMap.empty


insert :: forall a. a -> Memory -> ( Loc a, Memory )
insert x (Memory ls vs) =
  ( l, m' )
  where
    n = length ls
    l = Loc n :: Loc a
    m' = Memory
      (pack l : ls)
      (IntMap.insert n (pack x) vs)


lookup :: forall a. Loc a -> Memory -> a
lookup (Loc n) (Memory  _ vs) =
  --XXX: unsafe things happen here...
  fromJust $ unpack <$> IntMap.lookup n vs


adjust :: forall a. (a -> a) -> Loc a -> Memory -> Memory
adjust f (Loc n) (Memory ls vs) =
  Memory ls $ IntMap.adjust f' n vs
  where
  --XXX: unsafe things happen here...
    f' = unpack >> f >> pack



-- Transformer -----------------------------------------------------------------


newtype MemT m a
  = MemT (StateT Memory m a)
  deriving (Functor, Applicative, Monad)

deriving instance Monad m => MonadState Memory (MemT m)


instance Monad m => MonadRef Loc (MemT m) where

  newRef :: a -> MemT m (Loc a)
  newRef x = do
    m <- get
    let ( l, m' ) = insert x m
    put m'
    pure l

  readRef :: Loc a -> MemT m a
  readRef l = do
    m <- get
    pure $ lookup l m

  writeRef :: Loc a -> a -> MemT m ()
  writeRef l x = do
    m <- get
    let m' = adjust (const x) l m
    put m'



runMemT :: MemT m a -> m ( a, Memory )
runMemT (MemT st) = runStateT st empty


evalMemT :: Monad m => MemT m a -> m a
evalMemT mem = fst <$> runMemT mem


execMemT :: Monad m => MemT m a -> m Memory
execMemT mem = snd <$> runMemT mem


type Mem = MemT Identity


runMem :: Mem a -> ( a, Memory )
runMem (MemT st) = runIdentity $ runStateT st empty


evalMem :: Mem a -> a
evalMem mem = fst $ runMem mem


execMem :: Mem a -> Memory
execMem mem = snd $ runMem mem
