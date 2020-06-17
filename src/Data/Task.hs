module Data.Task
  ( -- * Types
    Task (..),
    Editor (..),
    Name (..),
    Label,

    -- * Constructors

    -- ** Editors
    enter,
    update,
    view,
    select,

    -- ** Shares
    share,
    watch,
    change,
    (<<-),
    (<<=),

    -- ** Derived
    parallel,
    choose,
    branch,
    eval,
    assert,
    (<?>),
    (>>?),
    (>>*),
    (>**),
    forever,
    (>>@),

    -- * Reexports
    module Data.Basic,
    module Data.Store,
    module Data.Some,
  )
where

import Data.Basic
import Data.Some
import Data.Store

---- Tasks ---------------------------------------------------------------------

-- | Tasks parametrised over a heap `h`, making use of effects `r`.
-- |
-- | **Important!**
-- | We do *not* encode this as a free monad.
-- | It is not free, because we'd like to have full control over the semantics for bind, i.e. `Step` below.
-- | This saves us from higher order interpretation,
-- | and gives us the freedom to completely control our own semantics.
-- |
-- | It can be seen best like this:
-- | We use `Sem` and its effects to *implement* its semantics,
-- | but `Task` is not an effect itself.
-- | In particular, it can't be combined with other effects,
-- | it only *needs* effects to be interpreted (denoted by `r`).
-- | I.e. `Task` is a monad on it's own right.
-- | (Although it actually isn't a monad... but that's another story.)
data Task h t where
  ---- Editors

  -- | Editors, named and unnamed
  Editor :: Name -> Editor h t -> Task h t
  ---- Parallels

  -- | Composition of two tasks.
  Pair :: Task h a -> Task h b -> Task h (a, b)
  -- | Internal, unrestricted and hidden editor
  Done :: t -> Task h t
  -- | Internal choice between two tasks.
  Choose :: Task h t -> Task h t -> Task h t
  -- | The failing task
  Fail :: Task h t
  ---- Steps

  -- | Internal value transformation
  Trans :: (a -> t) -> Task h a -> Task h t
  -- | Internal, or system step.
  Step :: Task h a -> (a -> Task h t) -> Task h t
  ---- Checks

  -- | Branching
  Branch :: List (Bool, Task h t) -> Task h t
  -- | Assertions
  Assert :: Bool -> Task h Bool
  ---- References
  -- The inner monad `m` needs to have the notion of references.
  -- These references should be `Eq` and `Typeable`,
  -- because we need to mark them dirty and match those with watched references.

  -- | Create new reference of type `t`
  Share :: (Basic t, Reflect h) => t -> Task h (Store h t)
  -- | Assign to a reference of type `t` to a given value
  Assign :: (Basic a) => a -> Store h a -> Task h ()

-- NOTE:
-- We could choose to replace `Share` and `Assign` and with a general `Lift` constructor,
-- taking an arbitrary action in the underlying monad `m`.
-- This action would then be performed during normalisation.
-- However, for now, we like to constrain the actions one can perform in the `Task` monad.
-- This makes actions like logging to stdout, lounching missiles or other effects impossible.
-- (Though this would need to be constrained with classes when specifying the task!)

data Editor h t where
  -- | Unvalued editor
  Enter :: (Basic t) => Editor h t
  -- | Valued editor
  Update :: (Basic t) => t -> Editor h t
  -- | Valued, view only editor
  View :: (Basic t) => t -> Editor h t
  -- | External choice between multple tasks.
  Select :: HashMap Label (Task h t) -> Editor h t
  -- | Change to a reference of type `t` to a value
  Change :: (Basic t) => Store h t -> Editor h t
  -- | Watch a reference of type `t`
  Watch :: (Basic t) => Store h t -> Editor h t

data Name
  = Unnamed
  | Named Nat
  deriving (Eq, Ord, Debug, Scan)

type Label =
  Text

new :: Editor h t -> Task h t
new e = Editor Unnamed e

---- Editors -------------------------------------------------------------------

enter :: (Basic t) => Task h t
enter = new Enter

update :: (Basic t) => t -> Task h t
update v = new (Update v)

view :: (Basic t) => t -> Task h t
view v = new (View v)

select :: HashMap Label (Task h t) -> Task h t
select ts = new (Select ts)

---- Shares --------------------------------------------------------------------

share :: (Basic a, Reflect h) => a -> Task h (Store h a)
share = Share

watch :: (Basic a) => Store h a -> Task h a
watch l = new (Watch l)

change :: (Basic a) => Store h a -> Task h a
change l = new (Change l)

infixl 1 <<-

infixl 1 <<=

(<<-) :: (Basic a) => Store h a -> a -> Task h ()
(<<-) = flip Assign

-- (<<=) :: (Members '[Read h, Write h] r) => Store h a -> (a -> a) -> Sem r ()
(<<=) :: (Basic a) => Store h a -> (a -> a) -> Task h ()
(<<=) r f = do
  x <- watch r
  r <<- f x

---- Derived forms -------------------------------------------------------------

parallel :: List (Task h a) -> Task h (List a)
parallel [] = pure []
parallel (t : ts) = t >< parallel ts >>= \(x, xs) -> pure (x : xs)

choose :: List (Task h a) -> Task h a
choose = foldr (<|>) fail

branch :: List (Bool, Task h a) -> Task h a
branch = Branch

assert :: Bool -> Task h Bool
assert = Assert

infixl 3 <?>

(<?>) :: Task h a -> Task h a -> Task h a
(<?>) t1 t2 = select ["Left" ~> t1, "Right" ~> t2]

infixl 1 >>?

(>>?) :: Task h a -> (a -> Task h b) -> Task h b
(>>?) t1 e2 = t1 >>= \x -> select ["Continue" ~> e2 x]

infixl 1 >>*

(>>*) :: Task h a -> HashMap Label (a -> Task h b) -> Task h b
(>>*) t1 cs = t1 >>= \x -> select (cs ||> \c -> c x)

infixl 1 >**

(>**) :: Task h a -> HashMap Label (a -> Bool, a -> Task h b) -> Task h b
(>**) t1 cs = t1 >>= \x -> select (cs ||> \(b, c) -> if b x then c x else fail)

forever :: Task h a -> Task h Void
forever t1 = t1 >>= \_ -> forever t1

(>>@) :: Task h a -> (a -> Task h b) -> Task h b
(>>@) t1 e2 = t1 >>= \x -> select ["Repeat" ~> t1 >>@ e2, "Exit" ~> e2 x]

---- Helpers -------------------------------------------------------------------

eval :: List (Bool, Task h a) -> Task h a
eval [] = fail
eval ((b, t) : rs) = if b then t else eval rs

---- Display -------------------------------------------------------------------

instance Display (Task h t) where
  display = \case
    Editor n e -> concat [display e |> between '(' ')', "^", display n]
    Pair t1 t2 -> unwords [display t1, "><", display t2] |> between '(' ')'
    Done _ -> "Done _"
    Choose t1 t2 -> unwords [display t1, "<|>", display t2] |> between '(' ')'
    Fail -> "Fail"
    Trans _ t -> unwords ["Trans _", display t]
    Step t _ -> unwords [display t, ">>=", "_"] |> between '(' ')'
    Branch ts -> unwords ["Branch", display ts]
    Assert b -> unwords ["Assert", display b]
    Share v -> unwords ["Share", display v]
    Assign v _ -> unwords ["_", ":=", display v]

instance Display (Editor h a) where
  display = \case
    Enter -> "Enter"
    Update v -> unwords ["Update", display v] |> between '(' ')'
    View v -> unwords ["View", display v] |> between '(' ')'
    Select ts -> unwords ["Select", display ts] |> between '(' ')'
    Watch _ -> unwords ["Watch", "_"]
    Change _ -> unwords ["Change", "_"]

instance Display Name where
  display = \case
    Unnamed -> "ε"
    Named n -> display n

---- Instances -----------------------------------------------------------------

instance Functor (Task h) where
  fmap = Trans

instance Monoidal (Task h) where
  (><) = Pair
  skip = Done ()

instance Applicative (Task h) where
  pure = Done
  (<*>) = applyDefault

-- instance Selective (Task h) where
--   branch p t1 t2 = go =<< p
--     where
--       go (Left a) = map (<| a) t1
--       go (Right b) = map (<| b) t2

instance Alternative (Task h) where
  (<|>) = Choose
  empty = Fail

instance Monad (Task h) where
  (>>=) = Step
