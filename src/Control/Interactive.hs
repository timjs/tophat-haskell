module Control.Interactive
  ( Interactive (..),
    Label,
  )
where

import Data.Basic
import qualified Data.HashMap.Strict as HashMap

-- | A monad `m` with interactive abilities.
-- |
-- | * `enter` lets the user enter a value
-- | * `update` lets the user view a value and asks him/her to enter a new one
-- | * `view` lets the user view a value
-- | * `select` asks the user to select one of possible options
-- |
-- | FIXME: Simplify constraint?
class (Monad m) => Interactive m where
  enter :: Basic a => m a
  update :: Basic a => a -> m a
  view :: Basic a => a -> m a

  select :: HashMap Label (m a) -> m a
  select ms = do
    _ <- view <| HashMap.keys ms
    l <- enter
    case HashMap.lookup l ms of
      Just m -> m
      Nothing -> select ms

type Label = Text

---- Example instance for IO ---------------------------------------------------

instance Interactive IO where
  enter :: forall a. Basic a => IO a
  enter = do
    putText <| display tau ++ "? "
    t <- getTextLn
    case scan t of
      Just x -> pure x
      Nothing -> enter
    where
      tau = typeRep :: TypeRep a

  update :: forall a. Basic a => a -> IO a
  update x = do
    _ <- view x
    enter

  view :: forall a. Basic a => a -> IO a
  view x = do
    putTextLn <| display x
    pure x
