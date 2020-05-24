module Data.Task
  ( Task (..),
    Editor (..),
    Name (..),
    Label,
    parallel,
    choose,
    branch,
    (<?>),
    (>>?),
    (>>*),
    (>**),
    forever,
    (>>@),
    -- module Control.Interactive,
    -- module Control.Collaborative,
    module Data.Basic,
    module Data.Someref,
    module Data.Store,
  )
where

-- import Control.Collaborative

import Control.Interactive
import Data.Basic
import Data.Heap (Heap)
import Data.Someref
import Data.Store
import qualified Data.Text.Prettyprint.Doc as Pretty

-- Tasks -----------------------------------------------------------------------

-- | Task effect build on top of a monad `m`.
data Task (h :: Heap h') (m :: Type -> Type) (t :: Type) where
  --
  -- Editors --
  Editor :: Name -> Editor h m t -> Task h m t
  --
  -- Parallels --

  -- | Composition of two tasks.
  Pair :: Task h m a -> Task h m b -> Task h m (a, b)
  -- | Internal, unrestricted and hidden editor
  Done :: t -> Task h m t
  -- | Internal choice between two tasks.
  Choose :: Task h m t -> Task h m t -> Task h m t
  -- | The failing task
  Fail :: Task h m t
  --
  -- Steps --

  -- | Internal value transformation
  Trans :: (a -> t) -> Task h m a -> Task h m t
  -- | Internal, or system step.
  Step :: Task h m a -> (a -> Task h m t) -> Task h m t
  --
  -- References --
  -- The inner monad `m` needs to have the notion of references.
  -- These references should be `Eq` and `Typeable`,
  -- because we need to mark them dirty and match those with watched references.

  -- | Create new reference of type `t`
  Share :: (Inspectable h t, Basic t) => t -> Task h m (Store h t)
  -- | Assign to a reference of type `t` to a given value
  Assign :: (Basic a) => a -> Store h a -> Task h m ()

-- NOTE:
-- We could choose to replace `Share` and `Assign` and with a general `Lift` constructor,
-- taking an arbitrary action in the underlying monad `m`.
-- This action would then be performed during normalisation.
-- However, for now, we like to constrain the actions one can perform in the `Task` monad.
-- This makes actions like logging to stdout, lounching missiles or other effects impossible.
-- (Though this would need to be constrained with classes when specifying the task!)

data Editor (h :: Heap h') (m :: Type -> Type) (t :: Type) where
  -- | Unvalued editor
  Enter :: (Basic t) => Editor h m t
  -- | Valued editor
  Update :: (Basic t) => t -> Editor h m t
  -- | Valued, view only editor
  View :: (Basic t) => t -> Editor h m t
  -- | External choice between multple tasks.
  Select :: HashMap Label (Task h m t) -> Editor h m t
  -- | Change to a reference of type `t` to a value
  Change :: (Inspectable h t, Basic t) => Store h t -> Editor h m t
  -- | Watch a reference of type `t`
  Watch :: (Inspectable h t, Basic t) => Store h t -> Editor h m t

data Name
  = Unnamed
  | Named Nat
  deriving (Eq, Ord, Show, Scan)

new :: Editor h m t -> Task h m t
new e = Editor Unnamed e

-- Derived forms ---------------------------------------------------------------

parallel :: List (Task h m a) -> Task h m (List a)
parallel [] = pure []
parallel (t : ts) = t >< parallel ts >>= \(x, xs) -> pure (x : xs)

choose :: List (Task h m a) -> Task h m a
choose = foldr (<|>) fail

branch :: List (Bool, Task h m a) -> Task h m a
branch [] = fail
branch ((b, t) : rs) = if b then t else branch rs

infixl 3 <?>

(<?>) :: Task h m a -> Task h m a -> Task h m a
(<?>) t1 t2 = select ["Left" ~> t1, "Right" ~> t2]

infixl 1 >>?

(>>?) :: Task h m a -> (a -> Task h m b) -> Task h m b
(>>?) t1 e2 = t1 >>= \x -> select ["Continue" ~> e2 x]

infixl 1 >>*

(>>*) :: Task h m a -> HashMap Label (a -> Task h m b) -> Task h m b
(>>*) t1 cs = t1 >>= \x -> select (cs ||> \c -> c x)

infixl 1 >**

(>**) :: Task h m a -> HashMap Label (a -> Bool, a -> Task h m b) -> Task h m b
(>**) t1 cs = t1 >>= \x -> select (cs ||> \(b, c) -> if b x then c x else fail)

forever :: Task h m a -> Task h m Void
forever t1 = t1 >>= \_ -> forever t1

(>>@) :: Task h m a -> (a -> Task h m b) -> Task h m b
(>>@) t1 e2 = t1 >>= \x -> select ["Repeat" ~> t1 >>@ e2, "Exit" ~> e2 x]

-- Pretty ----------------------------------------------------------------------

instance Pretty (Task h m t) where
  pretty = \case
    Editor n e -> cat [pretty e |> Pretty.parens, "^", pretty n]
    Pair t1 t2 -> sep [pretty t1, "><", pretty t2] |> Pretty.parens
    Done _ -> "Done _"
    Choose t1 t2 -> sep [pretty t1, "<|>", pretty t2] |> Pretty.parens
    Fail -> "Fail"
    Trans _ t -> sep ["Trans _", pretty t]
    Step t _ -> sep [pretty t, ">>=", "_"] |> Pretty.parens
    Share v -> sep ["Share", pretty v]
    Assign v _ -> sep ["_", ":=", pretty v]

instance Pretty (Editor h m a) where
  pretty = \case
    Enter -> "Enter"
    Update v -> sep ["Update", pretty v] |> Pretty.parens
    View v -> sep ["View", pretty v] |> Pretty.parens
    Select ts -> sep ["Select", pretty ts] |> Pretty.parens
    Watch _ -> sep ["Watch", "_"]
    Change _ -> sep ["Change", "_"]

instance Pretty Name where
  pretty = \case
    Unnamed -> "ε"
    Named n -> pretty n

-- Instances -------------------------------------------------------------------

instance Functor (Task h m) where
  fmap = Trans

instance Interactive (Task h m) where
  enter = new Enter
  update v = new (Update v)
  view v = new (View v)
  select ts = new (Select ts)

instance Monoidal (Task h m) where
  (><) = Pair
  skip = Done ()

instance Applicative (Task h m) where
  pure = Done
  (<*>) = applyDefault

-- instance Selective (Task h m) where
--   branch p t1 t2 = go =<< p
--     where
--       go (Left a) = map (<| a) t1
--       go (Right b) = map (<| b) t2

instance Alternative (Task h m) where
  (<|>) = Choose
  empty = Fail

instance Monad (Task h m) where
  (>>=) = Step

-- instance Collaborative r m => Collaborative r (Task h m) where
--   share = Share
--   assign = Assign
--   watch l = new (Watch l)
--   change l = new (Change l)
