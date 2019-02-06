module Data.Task.Internal
  ( TaskT(..)
  , Label
  , edit, enter, view, update, watch
  , lift, (-&&-), (&&-), (-&&), (-||-), (-??-), failure, (>>-), (>>?)
  , label, (-#-), delabel, keeper
  , module Control.Monad.Ref
  , module Data.Basic
  ) where


import Preload

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances.Text ()

import Control.Monad.Ref

import Data.Basic (Basic)


infixr 6 -#-
infixl 5 -&&-, -&&, &&-
infixl 3 -||-, -??-
infixl 2 >>-, >>?



-- Data ------------------------------------------------------------------------


type Label = Text


data TaskT :: (Type -> Type) -> Type -> Type where
  -- | Editors, valued or unvalued
  Edit :: Basic r => Maybe r -> TaskT m r

  -- | Stores referring to some shared value of type `r`
  Store :: ( MonadRef l m, Basic r ) => l r -> TaskT m r

  -- | Composition of two tasks.
  And :: TaskT m a -> TaskT m b -> TaskT m ( a, b )

  -- | Internal choice between two tasks.
  Or :: TaskT m r -> TaskT m r -> TaskT m r

  -- | External choice between two tasks.
  Xor :: TaskT m r -> TaskT m r -> TaskT m r

  -- | The failing task
  Fail :: TaskT m r

  -- | Internal, or system step.
  Then :: TaskT m a -> (a -> TaskT m r) -> TaskT m r

  -- | External, or user step.
  Next :: TaskT m a -> (a -> TaskT m r) -> TaskT m r

  -- | Labeled tasks.
  Label :: Label -> TaskT m r -> TaskT m r

  --FIXME: add lift and program some examples

-- Instances -------------------------------------------------------------------

-- Show --


instance Show (TaskT m a) where
  showsPrec d (Edit (Just x)) =
    showParen (d > p) $ showString "Edit " << showsPrec (succ p) x
      where p = 10
  showsPrec d (Edit Nothing) =
    showParen (d > p) $ showString "Edit _"
      where p = 10
  showsPrec d (Store _) =
    showParen (d > p) $ showString "Store"
      where p = 10
  showsPrec d (And left rght) =
    showParen (d > p) $ showsPrec (succ p) left << showString " -&&- " << showsPrec (succ p) rght
      where p = 5
  showsPrec d (Or left rght) =
    showParen (d > p) $ showsPrec (succ p) left << showString " -||- " << showsPrec (succ p) rght
      where p = 3
  showsPrec d (Xor left rght)  =
    case ( delabel left, delabel rght ) of
      ( Xor _ _, Xor _ _ ) -> showParen (d > p) $               showsPrec (succ p) left << showString " -??- " << showsPrec (succ p) rght
      ( Xor _ _, _ )       -> showParen (d > p) $               showsPrec (succ p) left << showString " -??- " << showText (fromMaybe "…" $ label rght)
      ( _, Xor _ _ )       -> showParen (d > p) $ showText (fromMaybe "…" $ label left) << showString " -??- " << showsPrec (succ p) rght
      ( _, _ )             -> showParen (d > p) $ showText (fromMaybe "…" $ label left) << showString " -??- " << showText (fromMaybe "…" $ label rght)
      where p = 3
  showsPrec _ (Fail) =
    showString "Fail"
  showsPrec d (Then this _) =
    showParen (d > p) $ showsPrec (succ p) this << showString " >>- …"
      where p = 1
  showsPrec d (Next this _) =
    showParen (d > p) $ showsPrec (succ p) this << showString " >>? …"
      where p = 1
  showsPrec d (Label lbl this) =
    showParen (d > p) $ showText lbl << showString ": " << showsPrec (succ p) this
      where p = 9



-- Functor --


lift :: Basic b => (a -> b) -> TaskT m a -> TaskT m b
lift f t = (>>-) t (\x -> edit (f x))



-- Applicative --


edit :: Basic a => a -> TaskT m a
edit x = Edit (Just x)


enter :: Basic a => TaskT m a
enter = Edit Nothing


view :: Basic a => a -> TaskT m a
view = edit


update :: MonadRef l m => Basic a => l a -> TaskT m a
update = Store


watch :: MonadRef l m => Basic a => l a -> TaskT m a
watch = update


(-&&-) :: TaskT m a -> TaskT m b -> TaskT m ( a, b )
(-&&-) = And


(-&&) :: Basic a => TaskT m a -> TaskT m b -> TaskT m a
x -&& y = lift fst $ x -&&- y


(&&-) :: Basic b => TaskT m a -> TaskT m b -> TaskT m b
x &&- y = lift snd $ x -&&- y


-- apply' :: TaskT m (a -> b) -> TaskT m a -> TaskT m b
-- apply' ff fx = map (\(Tuple f x) -> f x) $ ff <&> fx



-- Alternative --


failure :: TaskT m a
failure = Fail


(-||-) :: TaskT m a -> TaskT m a -> TaskT m a
(-||-) = Or


(-??-) :: TaskT m a -> TaskT m a -> TaskT m a
(-??-) = Xor



-- Monad --


(>>-) :: TaskT m b -> (b -> TaskT m a) -> TaskT m a
(>>-) = Then


(>>?) :: TaskT m a -> (a -> TaskT m b) -> TaskT m b
(>>?) = Next


-- Labeling --


(-#-) :: Text -> TaskT m a -> TaskT m a
(-#-) = Label



-- Arbitrary --


instance Arbitrary (TaskT m Int) where
  arbitrary =
    Gen.oneof
      [ arbitrary >>= (pure << edit)
      -- , arbitrary >>= (pure . update)
      , mkpair
      , (-||-) <$> arbitrary <*> arbitrary
      , (-??-) <$> arbitrary <*> arbitrary
      , pure failure
      , (>>-) <$> (arbitrary :: Gen (TaskT m Int)) <*> arbitrary
      , (>>?) <$> (arbitrary :: Gen (TaskT m Int)) <*> arbitrary
      -- , Label <$> (arbitrary :: Gen Label) <*> arbitrary
      ]
    where
      mkpair = do
        l <- arbitrary
        r <- arbitrary
        let c = \( x, y ) -> edit (x + y)
        pure $ (l -&&- r) >>- c



-- Labels ----------------------------------------------------------------------


-- | Get the current label, if one
label :: TaskT m a -> Maybe Label
label (Label l _) = Just l
label _           = Nothing


-- | Remove as much labels as possible from a task.
-- |
-- | Usefull to deeply match task constructors while ignoring labels.
delabel :: TaskT m a -> TaskT m a
delabel (Label _ t) = delabel t
delabel t           = t


-- | Check if a task constructor keeps its label after stepping or loses it.
keeper :: TaskT m a -> Bool
keeper (Edit _)  = True
keeper (And _ _) = True
keeper (Fail)    = True
keeper _         = False
