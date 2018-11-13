module Data.Task
  ( TaskIO, TaskST, TaskT
  , Task --, runTask, evalTask, execTask
  , NotApplicable
  , ui, value, failing, inputs
  , normalise, initialise, handle, drive
  -- ** Constructors
  , edit, enter, update
  , lift, (-&&-), (&&-), (-&&), (-||-), (-??-), failure, (>>-), (>>?)
  , label, delabel, keeper
  -- ** Reexports
  , Label
  , Basic
  , MonadRef
  , MonadTrace
  , module Control.Monad.Mem
  ) where


import Preload

import Control.Monad.Trace
import Control.Monad.Mem

import Data.Task.Input
import Data.Task.Internal



-- Aliases ---------------------------------------------------------------------


type TaskIO = TaskT IO


type TaskST s = TaskT (ST s)


type Task = TaskT Mem


-- runTask :: Task a -> ( a, Heap )
-- runTask = runMem


-- evalTask :: Task a -> a
-- evalTask = evalMem


-- execTask :: Task a -> Heap
-- execTask = execMem



-- Observations ----------------------------------------------------------------


ui :: MonadRef l m => TaskT m a -> m Text
ui (Edit (Just x)) = pure $ "□(" <> show x <> ")"
ui (Edit Nothing)  = pure $ "□(_)"
ui (Store l) = do
  x <- deref l
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
ui (Fail) = pure $ "↯"
ui (Then this _) = do
  t <- ui this
  pure $ t <> " ▶…"
ui (Next this _) = do
  t <- ui this
  pure $ t <> " ▷…"
ui (Label l this) = do
  t <- ui this
  pure $ l <> ":\n\t" <> t


value :: MonadRef l m => TaskT m a -> m (Maybe a)
value (Edit val)      = pure $ val
value (Store loc)     = Just <$> deref loc
value (And left rght) = (<&>) <$> value left <*> value rght
value (Or left rght)  = (<|>) <$> value left <*> value rght
value (Xor _ _)       = pure $ Nothing
value (Fail)          = pure $ Nothing
value (Then _ _)      = pure $ Nothing
value (Next _ _)      = pure $ Nothing
value (Label _ this)  = value this


failing :: TaskT m a -> Bool
failing (Edit _)        = False
failing (Store _)       = False
failing (And left rght) = failing left && failing rght
failing (Or  left rght) = failing left && failing rght
--TODO: fix to peek in future
failing (Xor left rght) = failing left && failing rght
failing (Fail)          = True
failing (Then this _)   = failing this
failing (Next this _)   = failing this
failing (Label _ this)  = failing this


inputs :: forall s l m a. Eq s => MonadZero m => MonadState s m => MonadRef l m => TaskT m a -> m (List (Input Dummy))
inputs (Edit _) =
  pure $ [ ToHere (AChange tau), ToHere AEmpty ]
  where
    tau = Proxy :: Proxy a
inputs (Store _) =
  pure $ [ ToHere (AChange tau) ]
  where
    tau = Proxy :: Proxy a
inputs (And left rght) = do
  l <- inputs left
  r <- inputs rght
  pure $ map ToLeft l ++ map ToRight r
inputs (Or left rght) = do
  l <- inputs left
  r <- inputs rght
  pure $ map ToLeft l ++ map ToRight r
inputs (Xor left rght) =
  pure $ map (ToHere << APick) choices
  where
    choices =
      case ( delabel left, delabel rght ) of
        ( Fail, Fail ) -> []
        ( _,    Fail ) -> [ GoLeft ]
        ( Fail, _ ) -> [ GoRight ]
        ( _,    _ ) -> [ GoLeft, GoRight ]
inputs (Fail) =
  pure $ []
inputs (Then this _) =
  inputs this
inputs (Next this next) =
  (++) <$> inputs this <*> [ [ToHere AContinue] | Just v <- value this, cont <- normalise (next v), not $ failing cont ]
inputs (Label _ this) =
  inputs this



-- Normalisation ---------------------------------------------------------------


stride :: MonadRef l m => TaskT m a -> m (TaskT m a)

-- Step --
stride (Then this cont) = do
  this_new <- stride this
  val <- value this_new
  case val of
    Nothing -> pure $ Then this_new cont
    Just v  ->
      --FIXME: should we use stride here instead of just eval?
      let next = cont v in
      if failing next then
        pure $ Then this_new cont
      else
        stride next

-- Evaluate --
stride (And left rght) = do
  left_new <- stride left
  rght_new <- stride rght
  pure $ And left_new rght_new

stride (Or left rght) = do
  left_new <- stride left
  val_left <- value left_new
  case val_left of
    Just _  -> pure $ left_new
    Nothing -> do
      rght_new <- stride rght
      val_rght <- value rght_new
      case val_rght of
        Just _  -> pure $ rght_new
        Nothing -> pure $ Or left_new rght_new

stride (Next this cont) = do
  this_new <- stride this
  pure $ Next this_new cont

-- Label --
stride (Label l this)
  | keeper this = stride this
  | otherwise = do
      this_new <- stride this
      pure $ Label l this_new

-- Values --
stride task = do
  pure $ task


normalise :: Eq s => MonadState s m => MonadRef l m => TaskT m a -> m (TaskT m a)
normalise task = do
  state_old <- get
  task_new <- stride task
  state_new <- get
  if state_old == state_new then
    pure task_new
  else
    normalise task_new


initialise :: Eq s => MonadState s m => MonadRef l m => TaskT m a -> m (TaskT m a)
initialise = normalise



-- Handling --------------------------------------------------------------------


data NotApplicable
  = CouldNotChange
  -- | CouldNotFind Label
  -- | CouldNotContinue
  | CouldNotHandle


handle :: forall l m a. MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Input Action -> m (TaskT m a)

-- Edit --
handle (Edit _) (ToHere Empty) =
  pure $ Edit Nothing

handle (Edit val) (ToHere (Change val_new))
  -- NOTE: Here we check if `val` and `val_new` have the same type.
  -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
  -- Because we can't acces the type variable `b` directly, we use `sameT` as a trick.
  | Just Refl <- sameT val val_new = pure $ Edit $ val_new
  | otherwise = trace CouldNotChange $ Edit val

handle (Store loc) (ToHere (Change val_ext))
  -- NOTE: As in the `Edit` case above, we check for type equality.
  -- Here, we can't annotate `Refl`, because we do not have acces to the type variable `b` inside `Store`.
  -- We also do not have acces to the value stored in `loc` (we could deref it first using `deref`).
  -- Therefore we use a proxy `Nothing` of the correct scoped type to mach against the type of `val_ext`.
  | Just Refl <- sameT (Nothing :: Maybe a) val_ext =
      case val_ext of
        Just val_new -> do
          loc $= const val_new
          pure $ Store loc
        Nothing ->
          trace CouldNotChange $ Store loc
  | otherwise =
      trace CouldNotChange $ Store loc

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
handle (Xor left _) (ToHere (Pick GoLeft)) =
  -- Go left
  --FIXME: add failing for left?
  pure $ delabel left

handle (Xor _ rght) (ToHere (Pick GoRight)) =
  -- Go rght
  --FIXME: add failing for rght?
  pure $ delabel rght

-- handle task@(Xor _ _) (ToHere (PickWith l)) =
--   case find l task of
--     Nothing -> trace (CouldNotFind l) task
--     --XXX: needs `assert_smaller` for totallity, be aware of long type checks...
--     -- Just p  -> handle task (assert_smaller (ToHere (PickWith l)) (ToHere (Pick p)))
--     Just p  -> handle task (ToHere (Pick p))

handle (Next this cont) (ToHere Continue) =
  -- When pressed continue rewrite to an internal step
  pure $ Then this cont

-- handle task@(Next this cont) (ToHere (ContinueWith l)) =
--   case !(value this) of
--     Nothing -> trace CouldNotContinue task
--     Just v  ->
--       let
--         next = cont v
--       in
--       case find l next of
--         Nothing -> trace (CouldNotFind l) task
--         Just p  -> handle next (ToHere (Pick p))

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
handle task _ =
  -- trace (CouldNotHandle input) task
  trace CouldNotHandle task


drive :: Eq s => MonadTrace NotApplicable m => MonadState s m => MonadRef l m => TaskT m a -> Input Action -> m (TaskT m a)
drive task input =
  handle task input >>= normalise
