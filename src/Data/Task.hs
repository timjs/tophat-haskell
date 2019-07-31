module Data.Task
  ( Task(..), Ref, Label
  , (<?>), (>>?), pick, forever
  , module Control.Interactive
  , module Control.Collaborative
  , module Data.Editable
  ) where


import Control.Interactive
import Control.Collaborative

import Data.Editable (Editable)

import qualified Data.HashMap.Strict as Dict


-- Tasks -----------------------------------------------------------------------

-- | Task monad transformer build on top of a monad `m`.
-- |
-- | To use references, `m` shoud be a `Collaborative` with locations `l`.
data Task (m :: Type -> Type) (r :: Type) where
  -- * Editors
  -- | Internal, unrestricted and hidden editor
  Done :: r -> Task m r
  -- | Unvalued editor
  Enter :: Editable r => Task m r
  -- | Valued editor
  Update :: Editable r => r -> Task m r
  -- | Valued, view only editor
  View :: Editable r => r -> Task m r

  -- * Parallels
  -- | Composition of two tasks.
  Pair :: Task m a -> Task m b -> Task m ( a, b )
  -- | Internal choice between two tasks.
  Choose :: Task m r -> Task m r -> Task m r
  -- | External choice between two tasks.
  Pick :: Dict Label (Task m r) -> Task m r
  -- | The failing task
  Fail :: Task m r

  -- * Transform
  -- | Internal value transformation
  Trans :: (a -> r) -> Task m a -> Task m r

  -- * Step
  -- | Internal, or system step.
  Step :: Task m a -> (a -> Task m r) -> Task m r

  -- * Loops
  -- | Repeat a task indefinitely
  Forever :: Task m r -> Task m Void

  -- * References
  -- | Create new reference of type `r`
  Share :: ( Collaborative l m, Editable r ) => r -> Task m (l r)
  -- | Assign to a reference of type `r` to a given value
  Assign :: ( Collaborative l m, Editable a ) => l a -> a -> Task m ()
  -- | Watch a reference of type `r`
  Watch :: ( Collaborative l m, Editable r ) => l r -> Task m r
  -- | Change to a reference of type `r` to a value
  Change :: ( Collaborative l m, Editable r ) => l r -> Task m r


type Ref = IORef
type Label = Text


-- Instances -------------------------------------------------------------------

infixl 3 <?>
infixl 1 >>?

(<?>) :: Task m a -> Task m a -> Task m a
(<?>) t1 t2 = pick [ ( "Left", t1 ), ( "Right", t2 ) ]

(>>?) :: Task m a -> (a -> Task m b) -> Task m b
(>>?) t c = t >>= \x -> pick [ ( "Continue", c x ) ]

pick :: List ( Label, Task m a ) -> Task m a
pick = Pick << Dict.fromList

forever :: Task m a -> Task m Void
forever = Forever

instance Pretty (Task m r) where
  pretty = \case
    Done _       -> "Done _"
    Enter        -> "Enter"
    Update v     -> parens <| sep [ "Update", pretty v ]
    View v       -> parens <| sep [ "View", pretty v ]

    Pair t1 t2   -> parens <| sep [ pretty t1, "<&>", pretty t2 ]
    Choose t1 t2 -> parens <| sep [ pretty t1, "<|>", pretty t2 ]
    Pick ts      -> parens <| sep [ "Pick", pretty ts ]
    Fail         -> "Fail"

    Trans _ t    -> sep [ "Trans _", pretty t ]
    Step t _     -> parens <| sep [ pretty t, ">>=", "_" ]
    Forever t    -> sep [ "Forever", pretty t ]

    Share v      -> sep [ "Share", pretty v ]
    Assign _ v   -> sep [ "_", ":=", pretty v ]
    Watch _      -> sep [ "Watch", "_" ]
    Change _     -> sep [ "Change", "_" ]

instance Functor (Task m) where
  fmap = Trans

instance Interactive (Task m) where
  enter  = Enter
  update = Update
  view   = View

instance Monoidal (Task m) where
  (<&>) = Pair
  skip  = Done ()

instance Applicative (Task m) where
  pure  = Done
  (<*>) = applyDefault

instance Selective (Task m) where
  branch p t1 t2 = go =<< p
    where
      go (Left  a) = map (<| a) t1
      go (Right b) = map (<| b) t2

instance Alternative (Task m) where
  (<|>) = Choose
  empty = Fail

instance Monad (Task m) where
  (>>=) = Step

instance Collaborative l m => Collaborative l (Task m) where
  share  = Share
  assign = Assign
  watch  = Watch
  change = Change
