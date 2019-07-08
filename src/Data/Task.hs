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
  -- * Transform
  Trans :: (a -> r) -> TaskT m a -> TaskT m r

  -- * Editors
  -- | Internal, unrestricted and hidden editor
  Done :: r -> TaskT m r
  -- | Unvalued editor
  Enter :: Editable r => TaskT m r
  -- | Valued editor
  Update :: Editable r => r -> TaskT m r
  -- | Valued, view only editor
  View :: Editable r => r -> TaskT m r

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
  New :: ( MonadRef l m, Storable r ) => r -> TaskT m (l r)
  -- | Watch a reference of type `r`
  Watch :: ( MonadRef l m, Storable r ) => l r -> TaskT m r
  -- | Change to a reference of type `r` to a value
  Change :: ( MonadRef l m, Storable a ) => l a -> a -> TaskT m ()

  -- * Loops
  -- Forever :: TaskT m r -> TaskT m Void


type Task = TaskT IO


-- Instances -------------------------------------------------------------------

instance Pretty (TaskT m t) where
  pretty = \case
    Trans _ x  -> cat [ "Trans _", pretty x ]
    Done _     -> "Done"
    Enter      -> "Enter"
    Update x   -> cat [ "Update", pretty x ]
    View x     -> cat [ "View", pretty x ]
    Pair x y   -> sep [ pretty x, "<&>", pretty y ]
    Choose x y -> sep [ pretty x, "<|>", pretty y ]
    Pick x y   -> sep [ pretty x, "<?>", pretty y ]
    Fail       -> "Fail"
    Bind x _   -> sep [ pretty x, ">>=", "_" ]
    New x      -> sep [ "New", pretty x ]
    Watch _    -> sep [ "Watch", "_" ]
    Change _ x -> sep [ "_", "<<-", pretty x ]

instance Functor (TaskT m) where
  fmap = Trans

instance Interactive (TaskT m) where
  enter  = Enter
  update = Update
  view   = View

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
  ref    = New
  deref  = Watch
  assign = Change


-- update :: MonadRef l m => Storable a => l a -> TaskT m a
-- update l = do
--   x <- deref l
--   l <<- x
--   deref l
