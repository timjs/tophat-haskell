module Data.Task.Internal
  ( TaskT(..)
  , Task
  , Label
  , edit, enter, update
  , map', pure', none', pair', fail', alt', (<?>), bind', (>>?)
  , label, delabel
  , module Control.Monad.Ref
  , module Data.Basic
  ) where


import Base

-- import Test.QuickCheck (Arbitrary, arbitrary)
-- import Test.QuickCheck.Gen as Gen

import Control.Monad.Ref (MonadRef, modify, new, read, write, (=:))

import Data.Basic (Basic)
import Data.IORef



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


type Task = TaskT IORef IO



-- Editors --


edit :: Basic a => a -> TaskT l m a
edit x = Edit (Just x)


enter :: Basic a => TaskT l m a
enter = Edit Nothing


update :: MonadRef l m => Basic a => l a -> TaskT l m a
update r = Store r



-- Instances -------------------------------------------------------------------

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



{- Arbitrary --


instance arbitraryTaskInt :: Arbitrary (TaskT l m Int) where
  arbitrary = fix arbitrary'
    where

      arbitrary' defered =
        Gen.oneOf $ NonEmpty
          ( pure fail' )
          [ arbitrary >>= (pure << edit)
          -- , arbitrary >>= (pure << update)
          , mkpair defered
          , alt' <$> defered <*> defered
          , (<?>) <$> defered <*> defered
          , bind' <$> defered <*> arbitrary
          , step' <$> defered <*> arbitrary
          , Label <$> arbitrary <*> defered
          ]

      mkpair defered = do
        l <- defered
        r <- defered
        let c = \(x ** y) -> edit (x + y)
        pure $ bind' (pair' l r) c



-- Labels ---------------------------------------------------------------------}


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
