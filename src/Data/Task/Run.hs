module Data.Task.Run where


import Control.Monad.Ref
import Control.Monad.Trace
import Control.Monad.Writer

import Data.List (union, intersect)
import Data.Task
import Data.Task.Input



-- Observations ----------------------------------------------------------------


ui :: MonadRef l m => TaskT m a -> m (Doc b)
ui = \case
  Done _     -> pure $ "⧇(_)"
  Edit v     -> pure $ sep [ "□(", pretty v, ")" ]
  Enter      -> pure "⊠(_)"
  Pair x y   -> pure (\l r -> sep [ l, " ⋈ ", r ]) <*> ui x <*> ui y
  Choose x y -> pure (\l r -> sep [ l, " ◆ ", r ]) <*> ui x <*> ui y
  Pick x y   -> pure (\l r -> sep [ l, " ◇ ", r ]) <*> ui x <*> ui y
  Empty      -> pure "↯"
  Bind x _   -> pure (<> " ▶…") <*> ui x
  Ref v      -> pure $ sep [ "ref", pretty v ]
  Deref l    -> pure (\x -> sep [ "■(", pretty x, ")" ]) <*> deref l
  Assign _ v -> pure $ sep [ "_", ":=", pretty v ]


value :: MonadRef l m => TaskT m a -> m (Maybe a)
value = \case
  Done v     -> pure (Just v)
  Edit v     -> pure (Just v)
  Enter      -> pure Nothing
  Pair x y   -> pure (<&>) <*> value x <*> value y
  Choose x y -> pure (<|>) <*> value x <*> value y
  Pick _ _   -> pure Nothing
  Empty      -> pure Nothing
  Bind _ _   -> pure Nothing
  Ref v      -> pure Just <*> ref v
  Deref l    -> pure Just <*> deref l
  Assign _ _ -> pure (Just ())


failing :: TaskT m a -> Bool
failing = \case
  Done _     -> False
  Edit _     -> False
  Enter      -> False
  Pair x y   -> failing x && failing y
  Choose x y -> failing x && failing y
  Pick x y   -> failing x && failing y
  Empty      -> True
  Bind x _   -> failing x
  Ref _      -> False
  Deref _    -> False
  Assign _ _ -> False


watching :: MonadRef l m => TaskT m a -> List (Someref m)
watching = \case
  Done _     -> []
  Edit _     -> []
  Enter      -> []
  Pair x y   -> watching x `union` watching y
  Choose x y -> watching x `union` watching y
  Pick _ _   -> []
  Empty      -> []
  Bind x _   -> watching x
  Ref _      -> []
  Deref l    -> [ pack l ]
  Assign l _ -> [ pack l ]


choices :: TaskT m a -> List Path
choices = \case
  Pick Empty Empty -> []
  Pick _    Empty  -> [ GoLeft ]
  Pick Empty _     -> [ GoRight ]
  Pick _    _      -> [ GoLeft, GoRight ]
  _                -> []


inputs :: forall m l a. MonadRef l m => TaskT m a -> m (List (Input Dummy))
inputs = \case
  Done _     -> pure []
  Edit _     -> pure [ ToHere (AChange tau), ToHere AEmpty ]
  Enter      -> pure [ ToHere (AChange tau) ]
  Pair x y   -> pure (\l r -> map ToFirst l <> map ToSecond r) <*> inputs x <*> inputs y
  Choose x y -> pure (\l r -> map ToFirst l <> map ToSecond r) <*> inputs x <*> inputs y
  Pick x y   -> pure $ map (ToHere << APick) (choices $ Pick x y)
  Empty      -> pure []
  Bind x _   -> inputs x
  Ref _      -> pure []
  Deref _    -> pure [ ToHere (AChange tau) ]
  Assign _ _ -> pure [ ToHere (AChange tau) ]
  where
    tau = Proxy :: Proxy a



-- Normalisation ---------------------------------------------------------------


stride :: MonadRef l m => MonadWriter (List (Someref m)) m => TaskT m a -> m (TaskT m a)
stride = \case
  -- * Step
  Bind x c -> do
    x' <- stride x
    vx <- value x'
    case vx of
      Nothing -> pure $ Bind x' c
      Just v  ->
        let y = c v in
        if failing y
          then pure $ Bind x' c
          --NOTE: We return just the next task. Normalisation should handle the next stride.
          else pure y
  -- * Choose
  Choose x y -> do
    x' <- stride x
    vx <- value x'
    case vx of
      Just _  -> pure x'
      Nothing -> do
        y' <- stride y
        vy <- value y'
        case vy of
          Just _  -> pure y'
          Nothing -> pure $ Choose x' y'
  -- * Evaluate
  Pair x y   -> pure Pair <*> stride x <*> stride y
  Ref v      -> pure $ ref v
  Deref l    -> pure $ deref l
  Assign l v -> tell [ pack l ] *> pure (l <<- v)
  -- * Ready
  x@(Done _)   -> pure x
  x@(Edit _)   -> pure x
  x@(Enter)    -> pure x
  x@(Pick _ _) -> pure x
  x@(Empty)    -> pure x


normalise :: MonadRef l m => MonadWriter (List (Someref m)) m => TaskT m a -> m (TaskT m a)
normalise x = do
  ( x', ds ) <- listen $ stride x
  clear
  let ws = watching x'
  case ds `intersect` ws of
    [] -> pure x'
    _  -> normalise x'


initialise :: MonadRef l m => MonadWriter (List (Someref m)) m => TaskT m a -> m (TaskT m a)
initialise = normalise



{- Handling --------------------------------------------------------------------


data NotApplicable
  = CouldNotChangeVal SomeTypeRep SomeTypeRep
  | CouldNotChangeRef SomeTypeRep SomeTypeRep
  -- | CouldNotFind Label
  -- | CouldNotContinue
  | CouldNotHandle (Input Action)


instance Pretty NotApplicable where
  pretty = \case
    CouldNotChangeVal v c -> sep [ "Could not change value because types", dquotes $ pretty v, "and", dquotes $ pretty c, "do not match" ]
    CouldNotChangeRef r c -> sep [ "Could not change value because cell", dquotes $ pretty r, "does not contain", dquotes $ pretty c ]
    -- CouldNotFind l   -> "Could not find label `" <> l <> "`"
    -- CouldNotContinue -> "Could not continue because there is no value to continue with"
    CouldNotHandle i -> sep [ "Could not handle input", dquotes $ pretty i ]


handle :: forall m l a.
  MonadTrace NotApplicable m => MonadRef l m =>
  TaskT m a -> Input Action -> m (TaskT m a)

-- Edit --
handle (Edit (Just _)) (ToHere Empty) =
  pure $ Edit Nothing

handle x@(Edit v) (ToHere (Change val_inp))
  -- NOTE: Here we check if `v` and `val_new` have the same type.
  -- If x is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
  -- Because we can't acces the type variable `b` directly, we use `~=` as a trick.
  | Just Refl <- v ~= val_new = pure $ Edit val_new
  | otherwise = trace (CouldNotChangeVal (someTypeOf v) (someTypeOf val_new)) x
  where
    --NOTE: `v` is of a maybe type, so we need to wrap `val_new` into a `Just`.
    val_new = Just val_inp

handle x@(Deref l) (ToHere (Change val_inp))
  -- NOTE: As in the `Edit` case above, we check for type equality.
  | Just Refl <- (typeRep :: TypeRep a) ~~ typeOf val_inp = do
      l $= const val_inp
      pure x
  | otherwise = trace (CouldNotChangeRef (someTypeOf l) (someTypeOf val_inp)) x

-- Interact --
handle x@(Pick x _) (ToHere (Pick GoLeft)) =
  if failing x
    then pure x
    else pure x

handle x@(Pick _ y) (ToHere (Pick GoRight)) =
  if failing y
    then pure x
    else pure y

handle (Next x c) (ToHere Continue) =
  -- TODO: When pressed continue, x rewrites to an internal step...
  pure $ Bind x c

-- Pass to x --
handle (Bind x c) input = do
  x' <- handle x input
  pure $ Bind x' c

handle (Next x c) input = do
  x' <- handle x input
  pure $ Next x' c

-- Pass to x or y --
handle (Pair x y) (ToFirst input) = do
  x' <- handle x input
  pure $ Pair x' y

handle (Pair x y) (ToSecond input) = do
  y' <- handle y input
  pure $ Pair x y'

handle (Choose x y) (ToFirst input) = do
  x' <- handle x input
  pure $ Choose x' y

handle (Choose x y) (ToSecond input) = do
  y' <- handle y input
  pure $ Choose x y'

-- Rest --
handle task input =
  trace (CouldNotHandle input) task


interact :: MonadTrace NotApplicable m => MonadRef l m => TaskT m a -> Input Action -> m (TaskT m a)
interact task input =
  handle task input >>= normalise



-- Running ---------------------------------------------------------------------


getUserInput :: IO (Input Action)
getUserInput = do
  putText ">> "
  input <- getLine
  case input of
    "quit" -> exitSuccess
    _ -> case parse (words input) of
      Right i -> pure i
      Left msg -> do
        print msg
        getUserInput


loop :: Task a -> IO ()
loop task = do
  interface <- ui task
  print interface
  events <- inputs task
  print $ "Possibilities: " <> pretty events
  input <- getUserInput
  task' <- interact task input
  loop task'


run :: Task a -> IO ()
run task = do
  task' <- initialise task
  loop task'

-}
