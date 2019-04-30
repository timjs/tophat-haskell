module Data.Task.Run where


import Control.Monad.Ref
import Control.Monad.Trace

import Data.List (union)
import Data.Task
import Data.Task.Input



-- Observations ----------------------------------------------------------------


ui :: MonadRef l m => TaskT m a -> m (Doc b)
ui = \case
  Edit (Just x) -> pure $ "□(" <> pretty x <> ")"
  Edit Nothing  -> pure "□(_)"
  Store loc     -> (\x -> "■(" <> pretty x <> ")") <$> deref loc
  And left rght -> (\l r -> l <> "   ⋈   " <> r) <$> ui left <*> ui rght
  Or left rght  -> (\l r -> l <> "   ◆   " <> r) <$> ui left <*> ui rght
  Xor left rght -> (\l r -> l <> "   ◇   " <> r) <$> ui left <*> ui rght
  Fail          -> pure "↯"
  Then this _   -> (<> " ▶…") <$> ui this
  Next this _   -> (<> " ▷…") <$> ui this


value :: MonadRef l m => TaskT m a -> m (Maybe a)
value = \case
  Edit val      -> pure val
  Store loc     -> Just <$> deref loc
  And left rght -> (<&>) <$> value left <*> value rght
  Or left rght  -> (<|>) <$> value left <*> value rght
  Xor _ _       -> pure Nothing
  Fail          -> pure Nothing
  Then _ _      -> pure Nothing
  Next _ _      -> pure Nothing


failing :: TaskT m a -> Bool
failing = \case
  Edit _        -> False
  Store _       -> False
  And left rght -> failing left && failing rght
  Or  left rght -> failing left && failing rght
  -- TODO: peek in future
  Xor left rght -> failing left && failing rght
  Fail          -> True
  Then this _   -> failing this
  Next this _   -> failing this


watching :: TaskT m a -> List (Someref m)
watching = \case
  Edit _        -> []
  Store loc     -> [ pack loc ]
  And left rght -> watching left `union` watching rght
  Or  left rght -> watching left `union` watching rght
  Xor  _   _    -> []
  Fail          -> []
  Then this _   -> watching this
  Next this _   -> watching this


choices :: TaskT m a -> List Path
choices = \case
  Xor Fail Fail -> []
  Xor _    Fail -> [ GoLeft ]
  Xor Fail _    -> [ GoRight ]
  Xor _    _    -> [ GoLeft, GoRight ]
  _             -> []


inputs :: forall m l a. MonadRef l m => TaskT m a -> m (List (Input Dummy))
inputs = \case
  Edit (Just _)  -> pure [ ToHere (AChange tau), ToHere AEmpty ]
  Edit Nothing   -> pure [ ToHere (AChange tau) ]
  Store _        -> pure [ ToHere (AChange tau) ]
  And left rght  -> (\l r -> map ToFirst l ++ map ToSecond r) <$> inputs left <*> inputs rght
  Or  left rght  -> (\l r -> map ToFirst l ++ map ToSecond r) <$> inputs left <*> inputs rght
  -- TODO: peek in future
  Xor left rght  -> pure $ map (ToHere << APick) (choices $ Xor left rght)
  Fail           -> pure []
  Then this _    -> inputs this
  Next this next -> do
    always <- inputs this
    val <- value this
    case val of
      Nothing -> pure always
      Just v  -> do
        cont <- normalise (next v)
        if not $ failing cont
          then pure $ ToHere AContinue : always
          else pure always
  where
    tau = Proxy :: Proxy a



-- Normalisation ---------------------------------------------------------------


stride :: MonadRef l m => TaskT m a -> m (TaskT m a)
stride = \case
  -- Step:
  Then this cont -> do
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
          --NOTE: We return just the next task. Normalisation should handle the next stride.
          pure next
  -- Choose:
  Or left rght -> do
    left_new <- stride left
    val_left <- value left_new
    case val_left of
      Just _  -> pure left_new
      Nothing -> do
        rght_new <- stride rght
        val_rght <- value rght_new
        case val_rght of
          Just _  -> pure rght_new
          Nothing -> pure $ Or left_new rght_new
  -- Evaluate:
  And left rght -> do
    left_new <- stride left
    rght_new <- stride rght
    pure $ And left_new rght_new
  Next this cont -> do
    this_new <- stride this
    pure $ Next this_new cont
  -- Lift --
  -- Lift act -> Edit << Just <$> act
  -- Values:
  task -> pure task


normalise :: MonadRef l m => TaskT m a -> m (TaskT m a)
-- TODO: stride till state changes
normalise = stride


initialise :: MonadRef l m => TaskT m a -> m (TaskT m a)
initialise = normalise



-- Handling --------------------------------------------------------------------


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

handle this@(Edit val) (ToHere (Change val_inp))
  -- NOTE: Here we check if `val` and `val_new` have the same type.
  -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
  -- Because we can't acces the type variable `b` directly, we use `~=` as a trick.
  | Just Refl <- val ~= val_new = pure $ Edit val_new
  | otherwise = trace (CouldNotChangeVal (someTypeOf val) (someTypeOf val_new)) this
  where
    --NOTE: `val` is of a maybe type, so we need to wrap `val_new` into a `Just`.
    val_new = Just val_inp

handle this@(Store loc) (ToHere (Change val_inp))
  -- NOTE: As in the `Edit` case above, we check for type equality.
  | Just Refl <- (typeRep :: TypeRep a) ~~ typeOf val_inp = do
      loc $= const val_inp
      pure this
  | otherwise = trace (CouldNotChangeRef (someTypeOf loc) (someTypeOf val_inp)) this

-- Interact --
handle this@(Xor left _) (ToHere (Pick GoLeft)) =
  if failing left
    then pure this
    else pure left

handle this@(Xor _ rght) (ToHere (Pick GoRight)) =
  if failing rght
    then pure this
    else pure rght

handle (Next this cont) (ToHere Continue) =
  -- TODO: When pressed continue, this rewrites to an internal step...
  pure $ Then this cont

-- Pass to this --
handle (Then this cont) input = do
  this_new <- handle this input
  pure $ Then this_new cont

handle (Next this cont) input = do
  this_new <- handle this input
  pure $ Next this_new cont

-- Pass to left or rght --
handle (And left rght) (ToFirst input) = do
  left_new <- handle left input
  pure $ And left_new rght

handle (And left rght) (ToSecond input) = do
  rght_new <- handle rght input
  pure $ And left rght_new

handle (Or left rght) (ToFirst input) = do
  left_new <- handle left input
  pure $ Or left_new rght

handle (Or left rght) (ToSecond input) = do
  rght_new <- handle rght input
  pure $ Or left rght_new

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
