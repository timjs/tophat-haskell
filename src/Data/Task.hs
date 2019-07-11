module Data.Task
  ( TaskT(..), Task, Ref, (<?>), (>>?)
  , module Control.Interactive
  , module Control.Collaborative
  , module Data.Editable
  ) where


import Control.Interactive
import Control.Collaborative

import Data.Editable (Editable)


-- Tasks -----------------------------------------------------------------------

-- | Task monad transformer build on top of a monad `m`.
-- |
-- | To use references, `m` shoud be a `Collaborative` with locations `l`.
data TaskT (m :: Type -> Type) (r :: Type) where
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

  -- * Transform
  -- | Internal value transformation
  Trans :: (a -> r) -> TaskT m a -> TaskT m r

  -- * Step
  -- | Internal, or system step.
  Step :: TaskT m a -> (a -> TaskT m r) -> TaskT m r

  -- * References
  -- | Create new reference of type `r`
  Store :: ( Collaborative l m, Editable r ) => r -> TaskT m (l r)
  -- | Assign to a reference of type `r` to a given value
  Assign :: ( Collaborative l m, Editable a ) => l a -> a -> TaskT m ()
  -- | Watch a reference of type `r`
  Watch :: ( Collaborative l m, Editable r ) => l r -> TaskT m r
  -- | Change to a reference of type `r` to a value
  Change :: ( Collaborative l m, Editable r ) => l r -> TaskT m r

  -- * Loops
  -- Forever :: TaskT m r -> TaskT m Void


type Task = TaskT IO
type Ref = IORef


-- Instances -------------------------------------------------------------------

infixl 3 <?>
infixl 1 >>?

(<?>) :: TaskT m a -> TaskT m a -> TaskT m a
(<?>) = Pick

(>>?) :: TaskT m a -> (a -> TaskT m b) -> TaskT m b
(>>?) t c = t >>= \x -> (c x) <?> empty

instance Pretty (TaskT m r) where
  pretty = \case
    Done _       -> "Done"
    Enter        -> "Enter"
    Update v     -> sep [ "Update", pretty v ]
    View v       -> sep [ "View", pretty v ]

    Pair t1 t2   -> sep [ pretty t1, "<&>", pretty t2 ]
    Choose t1 t2 -> sep [ pretty t1, "<|>", pretty t2 ]
    Pick t1 t2   -> sep [ pretty t1, "<?>", pretty t2 ]
    Fail         -> "Fail"

    Trans _ t    -> sep [ "Trans _", pretty t ]
    Step t _     -> sep [ pretty t, ">>=", "_" ]

    Store v      -> sep [ "Store", pretty v ]
    Assign _ v   -> sep [ "_", ":=", pretty v ]
    Watch _      -> sep [ "Watch", "_" ]
    Change _     -> sep [ "Change", "_" ]

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
  branch p t1 t2 = go =<< p
    where
      go (Left  a) = map (<| a) t1
      go (Right b) = map (<| b) t2

instance Alternative (TaskT m) where
  (<|>) = Choose
  empty = Fail

instance Monad (TaskT m) where
  (>>=) = Step

instance Collaborative l m => Collaborative l (TaskT m) where
  store  = Store
  assign = Assign
  watch  = Watch
  change = Change
