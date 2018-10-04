module Data.Task.Internal
  ( TaskT(..)
  , Label
  , edit, enter, update
  , map', pure', none', pair', fail', alt', (<?>), bind', (>>?)
  , label, delabel
  , module Control.Monad.Ref
  , module Data.Basic
  ) where


import Base

import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen
import Test.QuickCheck.Instances.Text ()

import Control.Monad.Ref (MonadRef, modify, new, read, write, (=:))

import Data.Basic (Basic)

import GHC.Show (Show(..))
import GHC.Show as GHC



-- Data ------------------------------------------------------------------------


type Label = Text


data TaskT :: (* -> *) -> (* -> *) -> * -> * where
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



-- Editors --


edit :: Basic a => a -> TaskT l m a
edit x = Edit (Just x)


enter :: Basic a => TaskT l m a
enter = Edit Nothing


update :: MonadRef l m => Basic a => l a -> TaskT l m a
update r = Store r



-- Instances -------------------------------------------------------------------

-- Show --

instance Show (TaskT l m a) where
  show (Edit (Just x)) = "□(" <> GHC.show x <> ")"
  show (Edit Nothing)  = "□(_)"
  show (Store _) = "■<loc>"
  show (And left rght) = GHC.show left <> "   ⋈   " <> GHC.show rght
  show (Or left rght) = GHC.show left <> "   ◆   " <> GHC.show rght
  show (Xor left rght) =
    case ( delabel left, delabel rght ) of
      ( Xor _ _, Xor _ _ ) -> GHC.show left <> " ◇ " <> GHC.show rght
      ( Xor _ _, _ ) -> GHC.show left <> " ◇ " <> (toS $ fromMaybe "…" (label rght))
      ( _, Xor _ _ ) -> (toS $ fromMaybe "…" (label left)) <> " ◇ " <> GHC.show rght
      ( _, _ ) -> (toS $ fromMaybe "…" (label left)) <> " ◇ " <> (toS $ fromMaybe "…" (label rght))
  show (Fail) = "↯"
  show (Then this _) = GHC.show this <> " ▶…"
  show (Next this _) = GHC.show this <> " ▷…"
  show (Label lbl this) = toS lbl <> ":\n\t" <> GHC.show this


-- Functor --


map' :: Basic b => (a -> b) -> TaskT l m a -> TaskT l m b
map' f t = bind' t (\x -> pure' (f x))



-- Applicative --


pure' :: Basic a => a -> TaskT l m a
pure' = edit


none' :: TaskT l m ()
none' = pure' ()


pair' :: TaskT l m a -> TaskT l m b -> TaskT l m ( a, b )
pair' = And


-- apply' :: TaskT l m (a -> b) -> TaskT l m a -> TaskT l m b
-- apply' ff fx = map (\(Tuple f x) -> f x) $ ff <&> fx



-- Alternative --


fail' :: TaskT l m a
fail' = Fail


alt' :: TaskT l m a -> TaskT l m a -> TaskT l m a
alt' = Or


infixl 3 <?>
(<?>) :: TaskT l m a -> TaskT l m a -> TaskT l m a
(<?>) = Xor



-- Monad --


bind' :: TaskT l m b -> (b -> TaskT l m a) -> TaskT l m a
bind' = Then


infixl 3 >>?
(>>?) :: TaskT l m a -> (a -> TaskT l m b) -> TaskT l m b
(>>?) = Next



-- Arbitrary --


instance Arbitrary (TaskT l m Int) where
  arbitrary =
    Gen.oneof
      [ arbitrary >>= (pure . edit)
      -- , arbitrary >>= (pure . update)
      , mkpair
      , alt' <$> arbitrary <*> arbitrary
      , (<?>) <$> arbitrary <*> arbitrary
      , pure fail'
      , bind' <$> (arbitrary :: Gen (TaskT l m Int)) <*> arbitrary
      , (>>?) <$> (arbitrary :: Gen (TaskT l m Int)) <*> arbitrary
      , Label <$> (arbitrary :: Gen Label) <*> arbitrary
      ]
    where
      mkpair = do
        l <- arbitrary
        r <- arbitrary
        let c = \( x, y ) -> edit (x + y)
        pure $ bind' (pair' l r) c



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
