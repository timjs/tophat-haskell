module Data.Task
  ( Task
  , ui
  , value
  ) where


import Base

import Data.IORef
import Data.Task.Internal



type Task = TaskT IORef IO



-- Observations ----------------------------------------------------------------


ui :: MonadRef l m => TaskT l m a -> m Text
ui (Edit (Just x)) = pure $ "□(" <> show x <> ")"
ui (Edit Nothing)  = pure $ "□(_)"
ui (Store l) = do
  x <- read l
  pure $ "■(" <> show x <> ")"
ui (And left rght) = do
  l <- ui left
  r <- ui rght
  pure $ l <> "   ⋈   " <> r
ui (Or left rght) = do
  l <- ui left
  r <- ui rght
  pure $ l <> "   ◆   " <> r
ui (Xor left rght) =
  case ( delabel left, delabel rght ) of
    ( Xor _ _, Xor _ _ ) -> do
      l <- ui left
      r <- ui rght
      pure $ l <> " ◇ " <> r
    ( Xor _ _, _ ) -> do
      l <- ui left
      pure $ l <> " ◇ " <> fromMaybe "…" (label rght)
    ( _, Xor _ _ ) -> do
      r <- ui rght
      pure $ fromMaybe "…" (label left) <> " ◇ " <> r
    ( _, _ ) ->
      pure $ fromMaybe "…" (label left) <> " ◇ " <> fromMaybe "…" (label rght)
ui (Fail) =
  pure $ "↯"
ui (Then this _) = do
  t <- ui this
  pure $ t <> " ▶…"
ui (Next this _) = do
  t <- ui this
  pure $ t <> " ▷…"
ui (Label l this) = do
  t <- ui this
  pure $ l <> ":\n\t" <> t


value :: MonadRef l m => TaskT l m a -> m (Maybe a)
value (Edit val) = pure $ val
value (Store loc) = Just <$> read loc
value (And left rght) = do
  l <- value left
  r <- value rght
  pure $ l <&> r
value (Or left rght) = do
  l <- value left
  r <- value rght
  pure $ l <|> r
value (Xor _ _) = pure $ Nothing
value (Fail) = pure $ Nothing
value (Then _ _) = pure $ Nothing
value (Next _ _) = pure $ Nothing
value (Label _ this) = value this
