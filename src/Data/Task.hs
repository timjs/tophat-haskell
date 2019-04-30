module Data.Task
  ( TaskT(..), Task
  , edit, enter, view, update, watch
  , tmap, tapply, (-&&-), (&&-), (-&&), (-||-), (-??-), fail, (>>-), (>>?)
  , module Control.Monad.Ref
  , module Data.Basic
  ) where


import Control.Monad.Ref

import Data.Basic (Basic)


infixl 5 -&&-, -&&, &&-
infixl 3 -||-, -??-
infixl 2 >>-, >>?



-- Tasks -----------------------------------------------------------------------


data TaskT (m :: Type -> Type) (r :: Type) where
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

  -- | Lift something to the underlaying monad
  -- Lift :: Monad m => m a -> TaskT m a

  --FIXME: add labels


type Task = TaskT IO



-- Instances -------------------------------------------------------------------

-- Show --

instance Pretty (TaskT m t) where
  pretty = \case
    Edit (Just x) -> cat [ "□(", pretty x, ")" ]
    Edit Nothing -> "□(_)"
    Store _ -> "■(_)"
    And x y -> sep [ pretty x, "⋈", pretty y ]
    Or x y -> sep [ pretty x, "◆", pretty y ]
    Xor x y -> sep [ pretty x, "◇", pretty y ]
    Fail -> "↯"
    Then x _ -> sep [ pretty x, "▶…" ]
    Next x _ -> sep [ pretty x, "▷…" ]



-- Functor --


tmap :: Basic b => (a -> b) -> TaskT m a -> TaskT m b
tmap f t = (>>-) t (edit << f)



-- Applicative --


edit :: Basic a => a -> TaskT m a
edit = Edit << Just


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
x -&& y = tmap fst $ x -&&- y


(&&-) :: Basic b => TaskT m a -> TaskT m b -> TaskT m b
x &&- y = tmap snd $ x -&&- y


tapply :: Basic b => TaskT m (a -> b) -> TaskT m a -> TaskT m b
tapply ff fx = tmap (\( f, x ) -> f x) $ ff -&&- fx



-- Alternative --


fail :: TaskT m a
fail = Fail


(-||-) :: TaskT m a -> TaskT m a -> TaskT m a
(-||-) = Or


(-??-) :: TaskT m a -> TaskT m a -> TaskT m a
(-??-) = Xor



-- Monad --


(>>-) :: TaskT m b -> (b -> TaskT m a) -> TaskT m a
(>>-) = Then


(>>?) :: TaskT m a -> (a -> TaskT m b) -> TaskT m b
(>>?) = Next



{- Arbitrary --


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

-}
