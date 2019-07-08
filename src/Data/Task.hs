module Data.Task
  ( TaskT(..), Task
  , module Control.Interactive
  , module Control.Monad.Ref
  , module Data.Editable
  ) where


import Control.Interactive
import Control.Monad.Ref

import Data.Editable (Editable, Storable)


-- Tasks -----------------------------------------------------------------------

-- | Task monad transformer build on top of a monad `m`.
-- |
-- | To use references, `m` shoud be a `MonadRef` with locations `l`.
data TaskT (m :: Type -> Type) (r :: Type) where
  -- * Editors
  -- | Internal, unrestricted and hidden editor
  Done :: r -> TaskT m r
  -- | Valued editors
  Update :: Editable r => r -> TaskT m r
  -- | Unvalued editors
  Enter :: Editable r => TaskT m r

  -- * Parallels
  -- | Composition of two tasks.
  Pair :: TaskT m a -> TaskT m b -> TaskT m ( a, b )
  -- | Internal choice between two tasks.
  Choose :: TaskT m r -> TaskT m r -> TaskT m r
  -- | External choice between two tasks.
  Pick :: TaskT m r -> TaskT m r -> TaskT m r
  -- | The failing task
  Fail :: TaskT m r

  -- * Steps
  -- | Internal, or system step.
  Bind :: TaskT m a -> (a -> TaskT m r) -> TaskT m r

  -- * References
  -- | Create new reference of type `r`
  Ref :: ( MonadRef l m, Storable r ) => r -> TaskT m (l r)
  -- | Deref a reference of type `r`
  Deref :: ( MonadRef l m, Storable r ) => l r -> TaskT m r
  -- | Assign to a reference of type `r`
  Assign :: ( MonadRef l m, Storable a ) => l a -> a -> TaskT m ()
  -- FIXME: use assign as update?

  --FIXME: add labels

type Task = TaskT IO


-- Instances -------------------------------------------------------------------

instance Pretty (TaskT m t) where
  pretty = \case
    Done _     -> "Done"
    Update x     -> cat [ "Update", pretty x ]
    Enter      -> "Enter"
    Pair x y   -> sep [ pretty x, "<&>", pretty y ]
    Choose x y -> sep [ pretty x, "<|>", pretty y ]
    Pick x y   -> sep [ pretty x, "<?>", pretty y ]
    Fail      -> "Fail"
    Bind x _   -> sep [ pretty x, ">>=", "_" ]
    Ref x      -> sep [ "Ref", pretty x ]
    Deref _    -> sep [ "Deref", "_" ]
    Assign _ x -> sep [ "_", "<<-", pretty x ]

instance Functor (TaskT m) where
  fmap g t = do
    x <- t
    pure $ g x

instance Interactive (TaskT m) where
  update = Update
  enter = Enter

instance Monoidal (TaskT m) where
  (<&>) = Pair
  skip  = Done ()

instance Applicative (TaskT m) where
  pure  = Done
  (<*>) = applyDefault

instance Selective (TaskT m) where
  branch p x y = go =<< p
    where
      go (Left  a) = map ($ a) x
      go (Right b) = map ($ b) y

instance Alternative (TaskT m) where
  (<|>) = Choose
  empty = Fail

instance Monad (TaskT m) where
  (>>=) = Bind

instance MonadRef l m => MonadRef l (TaskT m) where
  ref    = Ref
  deref  = Deref
  assign = Assign


-- update :: MonadRef l m => Storable a => l a -> TaskT m a
-- update l = do
--   x <- deref l
--   l <<- x
--   deref l
