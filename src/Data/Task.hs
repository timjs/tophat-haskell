module Data.Task
  ( Task (..),
    Editor (..),
    Name (..),
    parallel,
    choose,
    branch,
    (<?>),
    (>>?),
    (>>*),
    (>**),
    forever,
    (>>@),
    module Control.Interactive,
    module Control.Collaborative,
    module Data.Editable,
  )
where

import Control.Collaborative
import Control.Interactive
import Data.Editable (Editable)
import qualified Data.Text.Prettyprint.Doc as Pretty

---- Tasks ---------------------------------------------------------------------

-- | Task monad transformer build on top of a monad `m`.
-- |
-- | To use references, `m` shoud be a `Collaborative` with locations `r`.
data Task (m :: Type -> Type) (t :: Type) where
  ---- Editors ----

  -- | Editors, named or unnamed
  Editor :: Name -> Editor m t -> Task m t
  ---- Parallels ----

  -- | Composition of two tasks.
  Pair :: Task m a -> Task m b -> Task m (a, b)
  -- | Internal, unrestricted and hidden editor
  Done :: t -> Task m t
  -- | Internal choice between two tasks.
  Choose :: Task m t -> Task m t -> Task m t
  -- | The failing task
  Fail :: Task m t
  ---- Steps ----

  -- | Internal value transformation
  Trans :: (a -> t) -> Task m a -> Task m t
  -- | Internal, or system step.
  Step :: Task m a -> (a -> Task m t) -> Task m t
  ---- References ----
  -- The inner monad `m` needs to have the notion of references.
  -- These references should be `Eq` and `Typeable`,
  -- because we need to mark them dirty and match those with watched references.

  -- | Create new reference of type `t`
  Share :: (Collaborative r m, Editable t) => t -> Task m (Store r t)
  -- | Assign to a reference of type `t` to a given value
  Assign :: (Collaborative r m, Editable a) => a -> Store r a -> Task m ()

-- NOTE:
-- We could choose to replace `Share` and `Assign` and with a general `Lift` constructor,
-- taking an arbitrary action in the underlying monad `m`.
-- This action would then be performed during normalisation.
-- However, for now, we like to constrain the actions one can perform in the `Task` monad.
-- This makes actions like logging to stdout, lounching missiles or other effects impossible.
-- (Though this would need to be constrained with classes when specifying the task!)

data Editor (m :: Type -> Type) (t :: Type) where
  -- | Unvalued editor
  Enter :: (Editable t) => Editor m t
  -- | Valued editor
  Update :: (Editable t) => t -> Editor m t
  -- | Valued, view only editor
  View :: (Editable t) => t -> Editor m t
  -- | External choice between two Editors.
  Select :: HashMap Label (Task m t) -> Editor m t
  -- | Change to a reference of type `t` to a value
  Change :: (Collaborative r m, Editable t) => Store r t -> Editor m t
  -- | Watch a reference of type `t`
  Watch :: (Collaborative r m, Editable t) => Store r t -> Editor m t

data Name
  = Unnamed
  | Named Nat
  deriving (Eq, Ord, Show, Read)

new :: Editor m t -> Task m t
new e = Editor Unnamed e

---- Derived forms -------------------------------------------------------------

parallel :: List (Task m a) -> Task m (List a)
parallel [] = pure []
parallel (t : ts) = t >< parallel ts >>= \(x, xs) -> pure (x : xs)

choose :: List (Task m a) -> Task m a
choose = foldr (<|>) fail

branch :: List (Bool, Task m a) -> Task m a
branch [] = fail
branch ((b, t) : rs) = if b then t else branch rs

infixl 3 <?>

(<?>) :: Task m a -> Task m a -> Task m a
(<?>) t1 t2 = select ["Left" ~> t1, "Right" ~> t2]

infixl 1 >>?

(>>?) :: Task m a -> (a -> Task m b) -> Task m b
(>>?) t1 e2 = t1 >>= \x -> select ["Continue" ~> e2 x]

infixl 1 >>*

(>>*) :: Task m a -> HashMap Label (a -> Task m b) -> Task m b
(>>*) t1 cs = t1 >>= \x -> select (cs ||> \c -> c x)

infixl 1 >**

(>**) :: Task m a -> HashMap Label (a -> Bool, a -> Task m b) -> Task m b
(>**) t1 cs = t1 >>= \x -> select (cs ||> \(b, c) -> if b x then c x else fail)

forever :: Task m a -> Task m Void
forever t1 = t1 >>= \_ -> forever t1

(>>@) :: Task m a -> (a -> Task m b) -> Task m b
(>>@) t1 e2 = t1 >>= \x -> select ["Repeat" ~> t1 >>@ e2, "Exit" ~> e2 x]

---- Instances -----------------------------------------------------------------

instance Pretty (Task m t) where
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

instance Pretty (Editor m a) where
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

instance Functor (Task m) where
  fmap = Trans

instance Interactive (Task m) where
  enter = new Enter
  update v = new (Update v)
  view v = new (View v)
  select ts = new (Select ts)

instance Monoidal (Task m) where
  (><) = Pair
  skip = Done ()

instance Applicative (Task m) where
  pure = Done
  (<*>) = applyDefault

-- instance Selective (Task m) where
--   branch p t1 t2 = go =<< p
--     where
--       go (Left a) = map (<| a) t1
--       go (Right b) = map (<| b) t2

instance Alternative (Task m) where
  (<|>) = Choose
  empty = Fail

instance Monad (Task m) where
  (>>=) = Step

instance Collaborative r m => Collaborative r (Task m) where
  share = Share
  assign = Assign
  watch l = new (Watch l)
  change l = new (Change l)
