module Data.Task
  ( Act (..),
    Edit (..),
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

-- Acts ------------------------------------------------------------------------

-- | Act effect build on top of a monad `m`.
data Act (h :: Heap h') (m :: Type -> Type) (t :: Type) where
  --
  -- Editors --
  Edit :: Name -> Edit h m t -> Act h m t
  --
  -- Parallels --

  -- | Composition of two tasks.
  Pair :: Act h m a -> Act h m b -> Act h m (a, b)
  -- | Internal, unrestricted and hidden editor
  Done :: t -> Act h m t
  -- | Internal choice between two tasks.
  Choose :: Act h m t -> Act h m t -> Act h m t
  -- | The failing task
  Fail :: Act h m t
  --
  -- Steps --

  -- | Internal value transformation
  Trans :: (a -> t) -> Act h m a -> Act h m t
  -- | Internal, or system step.
  Step :: Act h m a -> (a -> Act h m t) -> Act h m t
  --
  -- References --
  -- The inner monad `m` needs to have the notion of references.
  -- These references should be `Eq` and `Typeable`,
  -- because we need to mark them dirty and match those with watched references.

  -- | Create new reference of type `t`
  Share :: (Basic t) => t -> Act h m (Store h t)
  -- | Assign to a reference of type `t` to a given value
  Assign :: (Basic a) => a -> Store h a -> Act h m ()

-- NOTE:
-- We could choose to replace `Share` and `Assign` and with a general `Lift` constructor,
-- taking an arbitrary action in the underlying monad `m`.
-- This action would then be performed during normalisation.
-- However, for now, we like to constrain the actions one can perform in the `Act` monad.
-- This makes actions like logging to stdout, lounching missiles or other effects impossible.
-- (Though this would need to be constrained with classes when specifying the task!)

data Edit (h :: Heap h') (m :: Type -> Type) (t :: Type) where
  -- | Unvalued editor
  Enter :: (Basic t) => Edit h m t
  -- | Valued editor
  Update :: (Basic t) => t -> Edit h m t
  -- | Valued, view only editor
  View :: (Basic t) => t -> Edit h m t
  -- | External choice between multple tasks.
  Select :: HashMap Label (Act h m t) -> Edit h m t
  -- | Change to a reference of type `t` to a value
  Change :: (Typeable (Store h t), Eq (Store h t), Basic t) => Store h t -> Edit h m t
  -- | Watch a reference of type `t`
  Watch :: (Typeable (Store h t), Eq (Store h t), Basic t) => Store h t -> Edit h m t

data Name
  = Unnamed
  | Named Nat
  deriving (Eq, Ord, Show, Scan)

new :: Edit h m t -> Act h m t
new e = Edit Unnamed e

-- Derived forms ---------------------------------------------------------------

parallel :: List (Act h m a) -> Act h m (List a)
parallel [] = pure []
parallel (t : ts) = t >< parallel ts >>= \(x, xs) -> pure (x : xs)

choose :: List (Act h m a) -> Act h m a
choose = foldr (<|>) fail

branch :: List (Bool, Act h m a) -> Act h m a
branch [] = fail
branch ((b, t) : rs) = if b then t else branch rs

infixl 3 <?>

(<?>) :: Act h m a -> Act h m a -> Act h m a
(<?>) t1 t2 = select ["Left" ~> t1, "Right" ~> t2]

infixl 1 >>?

(>>?) :: Act h m a -> (a -> Act h m b) -> Act h m b
(>>?) t1 e2 = t1 >>= \x -> select ["Continue" ~> e2 x]

infixl 1 >>*

(>>*) :: Act h m a -> HashMap Label (a -> Act h m b) -> Act h m b
(>>*) t1 cs = t1 >>= \x -> select (cs ||> \c -> c x)

infixl 1 >**

(>**) :: Act h m a -> HashMap Label (a -> Bool, a -> Act h m b) -> Act h m b
(>**) t1 cs = t1 >>= \x -> select (cs ||> \(b, c) -> if b x then c x else fail)

forever :: Act h m a -> Act h m Void
forever t1 = t1 >>= \_ -> forever t1

(>>@) :: Act h m a -> (a -> Act h m b) -> Act h m b
(>>@) t1 e2 = t1 >>= \x -> select ["Repeat" ~> t1 >>@ e2, "Exit" ~> e2 x]

-- Pretty ----------------------------------------------------------------------

instance Pretty (Act h m t) where
  pretty = \case
    Edit n e -> cat [pretty e |> Pretty.parens, "^", pretty n]
    Pair t1 t2 -> sep [pretty t1, "><", pretty t2] |> Pretty.parens
    Done _ -> "Done _"
    Choose t1 t2 -> sep [pretty t1, "<|>", pretty t2] |> Pretty.parens
    Fail -> "Fail"
    Trans _ t -> sep ["Trans _", pretty t]
    Step t _ -> sep [pretty t, ">>=", "_"] |> Pretty.parens
    Share v -> sep ["Share", pretty v]
    Assign v _ -> sep ["_", ":=", pretty v]

instance Pretty (Edit h m a) where
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

instance Functor (Act h m) where
  fmap = Trans

instance Interactive (Act h m) where
  enter = new Enter
  update v = new (Update v)
  view v = new (View v)
  select ts = new (Select ts)

instance Monoidal (Act h m) where
  (><) = Pair
  skip = Done ()

instance Applicative (Act h m) where
  pure = Done
  (<*>) = applyDefault

-- instance Selective (Act h m) where
--   branch p t1 t2 = go =<< p
--     where
--       go (Left a) = map (<| a) t1
--       go (Right b) = map (<| b) t2

instance Alternative (Act h m) where
  (<|>) = Choose
  empty = Fail

instance Monad (Act h m) where
  (>>=) = Step

-- instance Collaborative r m => Collaborative r (Act h m) where
--   share = Share
--   assign = Assign
--   watch l = new (Watch l)
--   change l = new (Change l)
