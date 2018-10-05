module Data.Task.Internal
  ( TaskT(..)
  , Label
  , edit, enter, update
  , tmap, (|&|), (|!|), (|?|), fail, (>>!), (>>?)
  , label, delabel, keeper
  , module Control.Monad.Ref
  , module Data.Basic
  ) where


import Preload

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances.Text ()

import Control.Monad.Ref (MonadRef(..))

import Data.Basic (Basic)

import GHC.Show (Show(showsPrec), ShowS, showString, showParen)


infixl 5 |&|
infixl 3 |!|, |?|
infixl 1 >>!, >>?



-- Data ------------------------------------------------------------------------


type Label = Text


data TaskT :: (Type -> Type) -> (Type -> Type) -> Type -> Type where
  -- | Editors, valued or unvalued
  Edit :: Basic r => Maybe r -> TaskT l m r

  -- | Stores refering to some shared value of type `r`
  Store :: ( MonadRef l m, Basic r ) => l r -> TaskT l m r

  -- | Composition of two tasks.
  -- |
  -- | Encoded in double negation style, conceptually equal to:
  -- |
  -- |     And (exist a b. TypeEquals r (Tuple a b) => Tuple (TaskT l m a) (TaskT l m b))
  -- |
  And :: TaskT l m a -> TaskT l m b -> TaskT l m ( a, b )

  -- | Internal choice between two tasks.
  Or :: TaskT l m r -> TaskT l m r -> TaskT l m r

  -- | External choice between two tasks.
  Xor :: TaskT l m r -> TaskT l m r -> TaskT l m r

  -- | The failing task
  Fail :: TaskT l m r

  -- | Internal, or system step.
  -- |
  -- | Encoded in double negation style, conceptually equal to:
  -- |
  -- |     Then (exist a. Tuple (TaskT l m a) (a -> TaskT l m r))
  -- |
  Then :: TaskT l m a -> (a -> TaskT l m r) -> TaskT l m r

  -- | External, or user step.
  -- |
  -- | Encoded in double negation style, conceptually equal to:
  -- |
  -- |     Next (exist a. Tuple (TaskT l m a) (a -> TaskT l m r))
  -- |
  Next :: TaskT l m a -> (a -> TaskT l m r) -> TaskT l m r

  -- | Labeled tasks.
  Label :: Label -> TaskT l m r -> TaskT l m r





-- Instances -------------------------------------------------------------------

-- Show --

instance Show (TaskT l m a) where
  showsPrec d (Edit (Just x)) =
    showParen (d > p) $ showString "Edit " . showsPrec (succ p) x
      where p = 10
  showsPrec d (Edit Nothing) =
    showParen (d > p) $ showString "Edit _"
      where p = 10
  showsPrec d (Store _) =
    showParen (d > p) $ showString "Store"
      where p = 10
  showsPrec d (And left rght) =
    showParen (d > p) $ showsPrec (succ p) left . showString " |&| " . showsPrec (succ p) rght
      where p = 5
  showsPrec d (Or left rght) =
    showParen (d > p) $ showsPrec (succ p) left . showString " |!| " . showsPrec (succ p) rght
      where p = 3
  showsPrec d (Xor left rght)  =
    case ( delabel left, delabel rght ) of
      ( Xor _ _, Xor _ _ ) -> showParen (d > p) $               showsPrec (succ p) left . showString " |?| " . showsPrec (succ p) rght
      ( Xor _ _, _ )       -> showParen (d > p) $               showsPrec (succ p) left . showString " |?| " . showText (fromMaybe "…" $ label rght)
      ( _, Xor _ _ )       -> showParen (d > p) $ showText (fromMaybe "…" $ label left) . showString " |?| " . showsPrec (succ p) rght
      ( _, _ )             -> showParen (d > p) $ showText (fromMaybe "…" $ label left) . showString " |?| " . showText (fromMaybe "…" $ label rght)
      where p = 3
  showsPrec _ (Fail) =
    showString "Fail"
  showsPrec d (Then this _) =
    showParen (d > p) $ showsPrec (succ p) this . showString " >>! …"
      where p = 1
  showsPrec d (Next this _) =
    showParen (d > p) $ showsPrec (succ p) this . showString " >>? …"
      where p = 1
  showsPrec d (Label lbl this) =
    showParen (d > p) $ showText lbl . showString ": " . showsPrec (succ p) this
      where p = 9


showText :: Text -> ShowS
showText = showString . toS



-- Functor --


tmap :: Basic b => (a -> b) -> TaskT l m a -> TaskT l m b
tmap f t = (>>!) t (\x -> edit (f x))



-- Applicative --


edit :: Basic a => a -> TaskT l m a
edit x = Edit (Just x)


enter :: Basic a => TaskT l m a
enter = Edit Nothing


update :: MonadRef l m => Basic a => l a -> TaskT l m a
update r = Store r


(|&|) :: TaskT l m a -> TaskT l m b -> TaskT l m ( a, b )
(|&|) = And


-- apply' :: TaskT l m (a -> b) -> TaskT l m a -> TaskT l m b
-- apply' ff fx = map (\(Tuple f x) -> f x) $ ff <&> fx



-- Alternative --


fail :: TaskT l m a
fail = Fail


(|!|) :: TaskT l m a -> TaskT l m a -> TaskT l m a
(|!|) = Or


(|?|) :: TaskT l m a -> TaskT l m a -> TaskT l m a
(|?|) = Xor



-- Monad --


(>>!) :: TaskT l m b -> (b -> TaskT l m a) -> TaskT l m a
(>>!) = Then


(>>?) :: TaskT l m a -> (a -> TaskT l m b) -> TaskT l m b
(>>?) = Next



-- Arbitrary --


instance Arbitrary (TaskT l m Int) where
  arbitrary =
    Gen.oneof
      [ arbitrary >>= (pure . edit)
      -- , arbitrary >>= (pure . update)
      , mkpair
      , (|!|) <$> arbitrary <*> arbitrary
      , (|?|) <$> arbitrary <*> arbitrary
      , pure fail
      , (>>!) <$> (arbitrary :: Gen (TaskT l m Int)) <*> arbitrary
      , (>>?) <$> (arbitrary :: Gen (TaskT l m Int)) <*> arbitrary
      -- , Label <$> (arbitrary :: Gen Label) <*> arbitrary
      ]
    where
      mkpair = do
        l <- arbitrary
        r <- arbitrary
        let c = \( x, y ) -> edit (x + y)
        pure $ (l |&| r) >>! c



-- Labels ----------------------------------------------------------------------


-- | Get the current label, if one
label :: TaskT l m a -> Maybe Label
label (Label l _) = Just l
label _           = Nothing


-- | Remove as much labels as possible from a task.
-- |
-- | Usefull to deeply match task constructors while ignoring labels.
delabel :: TaskT l m a -> TaskT l m a
delabel (Label _ t) = delabel t
delabel t           = t


-- | Check if a task constructor keeps its label after stepping or loses it.
keeper :: TaskT l m a -> Bool
keeper (Edit _)  = True
keeper (And _ _) = True
keeper (Fail)    = True
keeper _         = False
