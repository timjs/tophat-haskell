module Data.Task
  ( Task, TaskT
  , ui, value, failing
  , normalise --,initialise , handle, drive
  -- ** Constructors
  , edit, enter, update
  , tmap, (|&|), (|||), (|?|), fail, (>>-), (>>?)
  , label, delabel, keeper
  -- ** Reexports
  , Label
  , Basic
  , MonadRef
  ) where


import Preload

import Data.IORef
import Data.Task.Internal



type Task = TaskT IORef IO



-- Observations ----------------------------------------------------------------


ui :: MonadRef l m => TaskT l m a -> m Text
ui (Edit (Just x)) = pure $ "□(" <> show x <> ")"
ui (Edit Nothing)  = pure $ "□(_)"
ui (Store l) = do
  x <- readRef l
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
value (Store loc) = Just <$> readRef loc
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


failing :: TaskT l m a -> Bool
failing (Edit _)        = False
failing (Store _)       = False
failing (And left rght) = failing left && failing rght
failing (Or left rght)  = failing left && failing rght
failing (Xor left rght) = failing left && failing rght
failing (Fail)          = True
failing (Then this _)   = failing this
failing (Next this _)   = failing this
failing (Label _ this)  = failing this



-- Normalisation ---------------------------------------------------------------


normalise :: MonadRef l m => TaskT l m a -> m (TaskT l m a)

-- Step --
normalise (Then this cont) = do
  this_new <- normalise this
  val <- value this_new
  case val of
    Nothing -> pure $ Then this_new cont
    Just v  ->
      --FIXME: should we use normalise here instead of just eval?
      let next = cont v in
      if failing next then
        pure $ Then this_new cont
      else
        normalise next

-- Evaluate --
normalise (And left rght) = do
  left_new <- normalise left
  rght_new <- normalise rght
  pure $ And left_new rght_new

normalise (Or left rght) = do
  left_new <- normalise left
  val_left <- value left_new
  case val_left of
    Just _  -> pure $ left_new
    Nothing -> do
      rght_new <- normalise rght
      val_rght <- value rght_new
      case val_rght of
        Just _  -> pure $ rght_new
        Nothing -> pure $ Or left_new rght_new

normalise (Next this cont) = do
  this_new <- normalise this
  pure $ Next this_new cont

-- Label --
normalise (Label l this)
  | keeper this = normalise this
  | otherwise = do
      this_new <- normalise this
      pure $ Label l this_new

-- Values --
normalise task = do
  pure $ task


{- Input handling --------------------------------------------------------------


handle :: MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Input -> m (TaskT m a)

-- Edit --
handle (Edit _) (ToHere Empty) =
  pure $ Edit Nothing

handle (Edit {r} val) (ToHere (Change {c} val_new)) with (decEq c r)
  handle (Edit _)   (ToHere (Change val_new))       | Yes Refl = pure $ Edit $ toMaybe val_new
  handle (Edit val) (ToHere (Change _))             | No _     = trace CouldNotChange $ Edit val

handle (Store {r} loc) (ToHere (Change {c} val_new)) with (decEq c r)
  handle (Store loc) (ToHere (Change (Exactly val_new))) | Yes Refl = do
    loc := val_new
    pure $ Store loc
  handle (Store loc) (ToHere (Change (Anything)))        | Yes Refl = trace CouldNotChange $ Store loc
  handle (Store loc) (ToHere (Change _))                 | No _     = trace CouldNotChange $ Store loc

-- Pass to left or rght --
handle (And left rght) (ToLeft input) = do
  -- Pass the input to left
  left_new <- handle left input
  pure $ And left_new rght

handle (And left rght) (ToRight input) = do
  -- Pass the input to rght
  rght_new <- handle rght input
  pure $ And left rght_new

handle (Or left rght) (ToLeft input) = do
  -- Pass the input to left
  left_new <- handle left input
  pure $ Or left_new rght

handle (Or left rght) (ToRight input) = do
  -- Pass the input to rght
  rght_new <- handle rght input
  pure $ Or left rght_new

-- Interact --
handle task@(Xor left _) (ToHere (Pick (GoLeft p))) =
  -- Go left
  --FIXME: add failing for left?
  handle (assert_smaller task (delabel left)) (ToHere (Pick p))

handle task@(Xor _ rght) (ToHere (Pick (GoRight p))) =
  -- Go rght
  --FIXME: add failing for rght?
  handle (assert_smaller task (delabel rght)) (ToHere (Pick p))

handle task (ToHere (Pick GoHere)) =
  -- Go here
  pure $ task

handle task@(Xor _ _) (ToHere (PickWith l)) =
  case find l task of
    Nothing -> trace (CouldNotFind l) task
    --XXX: needs `assert_smaller` for totallity, be aware of long type checks...
    -- Just p  -> handle task (assert_smaller (ToHere (PickWith l)) (ToHere (Pick p)))
    Just p  -> handle task (ToHere (Pick p))

handle task@(Next this cont) (ToHere Continue) =
  -- When pressed continue rewrite to an internal step
  pure $ Then this cont

handle task@(Next this cont) (ToHere (ContinueWith l)) =
  case !(value this) of
    Nothing -> trace CouldNotContinue task
    Just v  ->
      let
        next = cont v
      in
      case find l next of
        Nothing -> trace (CouldNotFind l) task
        Just p  -> handle next (ToHere (Pick p))

-- Pass to this --
handle (Then this cont) input = do
  -- Pass the input to this
  this_new <- handle this input
  pure $ Then this_new cont

handle (Next this cont) input = do
  -- Pass the input to this
  this_new <- handle this input
  pure $ Next this_new cont

-- Label --
handle (Label l this) input
  | keeper this = handle this input
  | otherwise = do
      this_new <- handle this input
      pure $ Label l this_new

-- Rest --
handle task input =
  -- Case `Fail`: Evaluation continues indefinitely
  trace (CouldNotHandle input) task


drive :: MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Input -> m (TaskT m a)
drive task input =
  handle task input >>= normalise

-------------------------------------------------------------------------------}
