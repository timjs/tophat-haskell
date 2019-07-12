module Data.Task.Run where


import Control.Monad.Log

import Data.List (union, intersect)
import Data.Task
import Data.Task.Input

import qualified Data.HashMap.Strict as Dict
import qualified Data.HashSet as Set


-- Observations ----------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.


ui ::
  Collaborative l m =>
  TaskT m a -> m (Doc b)  -- We need to watch locations and be in monad `m`
ui = \case
  Done _       -> pure "■(…)"
  Enter        -> pure "⊠()"
  Update v     -> pure <| cat [ "□(", pretty v, ")" ]
  View v       -> pure <| cat [ "⧇(", pretty v, ")" ]

  Pair t1 t2   -> pure (\l r -> sep [ l, " ⧓ ", r ]) -< ui t1 -< ui t2
  Choose t1 t2 -> pure (\l r -> sep [ l, " ◆ ", r ]) -< ui t1 -< ui t2
  Pick ts      -> pure <| cat [ "◇", pretty <| Dict.keysSet ts ]
  Fail         -> pure "↯"

  Trans _ t    -> ui t
  Forever t    -> pure (\s -> cat [ "⟲", s ]) -< ui t
  Step t1 e2   -> pure (\s n -> cat [ s, " ▶", pretty n, "…" ]) -< ui t1 -< count
    where
      count = do
        mv1 <- value t1
        case mv1 of
          Nothing -> pure 0
          Just v1 -> pure <| length <| options (e2 v1)

  Store v      -> pure <| sep [ "store", pretty v ]
  Assign _ v   -> pure <| sep [ "…", ":=", pretty v ]
  Watch l      -> pure (\v -> cat [ "⧈(", pretty v, ")" ]) -< watch l
  Change l     -> pure (\v -> cat [ "⊟(", pretty v, ")" ]) -< watch l


value ::
  Collaborative l m =>
  TaskT m a -> m (Maybe a)  -- We need to watch locations and be in monad `m`
value = \case
  Done v       -> pure (Just v)
  Enter        -> pure Nothing
  Update v     -> pure (Just v)
  View v       -> pure (Just v)

  Pair t1 t2   -> pure (<&>) -< value t1 -< value t2
  Choose t1 t2 -> pure (<|>) -< value t1 -< value t2
  Pick _       -> pure Nothing
  Fail         -> pure Nothing

  Trans f t    -> pure (map f) -< value t
  Forever _    -> pure Nothing
  Step _ _     -> pure Nothing

  Store v      -> pure Just -< store v
  Assign _ _   -> pure (Just ())
  Watch l      -> pure Just -< watch l
  Change l     -> pure Just -< watch l


failing ::
  TaskT m a -> Bool
failing = \case
  Done _       -> False
  Enter        -> False
  Update _     -> False
  View _       -> False

  Pair t1 t2   -> failing t1 && failing t2
  Choose t1 t2 -> failing t1 && failing t2
  Pick ts      -> all failing ts
  Fail         -> True

  Trans _ t    -> failing t
  Forever t    -> failing t
  Step t _     -> failing t

  Store _      -> False
  Assign _ _   -> False
  Watch _      -> False
  Change _     -> False


watching ::
  Collaborative l m =>           -- We need Collaborative here to pack locations `l`
  TaskT m a -> List (Someref m)  -- But there is no need to be in monad `m`
watching = \case
  Done _       -> []
  Enter        -> []
  Update _     -> []
  View _       -> []

  Pair t1 t2   -> watching t1 `union` watching t2
  Choose t1 t2 -> watching t1 `union` watching t2
  Pick _       -> []
  Fail         -> []

  Trans _ t1   -> watching t1
  Forever t1   -> watching t1
  Step t1 _    -> watching t1

  Store _      -> []
  Assign _ _   -> []
  Watch l      -> [ pack l ]
  Change l     -> [ pack l ]


options ::
  TaskT m a -> Set Label
options = \case
  Pick ts    -> Dict.keysSet <| Dict.filter (not << failing) ts

  -- Trans _ t1 -> options t1
  -- Forever t1 -> options t1
  -- Step t1 _  -> options t1

  _ -> []


inputs :: forall m l a.
  Collaborative l m =>
  TaskT m a -> m (List (Input Dummy))  -- We need to call `value`, therefore we are in `m`
inputs t = case t of
  Done _       -> pure []
  Enter        -> pure [ ToHere (AChange tau) ]
  Update _     -> pure [ ToHere (AChange tau) ]
  View _       -> pure []

  Pair t1 t2   -> pure (\l r -> map ToFirst l <> map ToSecond r) -< inputs t1 -< inputs t2
  Choose t1 t2 -> pure (\l r -> map ToFirst l <> map ToSecond r) -< inputs t1 -< inputs t2
  Pick _       -> pure <| map (ToHere << APick) <| Set.toList <| options t
  Fail         -> pure []

  Trans _ t1   -> inputs t1
  Forever t1   -> inputs t1
  Step t1 e2   -> pure (<>) -< inputs t1 -< do
    mv1 <- value t1
    case mv1 of
      Nothing -> pure []
      Just v1 -> do
        -- t2 <- normalise (e2 v1)  --XXX: Do we need to normalise here??
        let t2 = e2 v1
        if failing t2
          then pure []
          else pure <| map (ToHere << AContinue) <| Set.toList <| options t2

  Store _      -> pure []
  Assign _ _   -> pure []
  Watch _      -> pure []
  Change _     -> pure [ ToHere (AChange tau) ]
  where
    tau = Proxy :: Proxy a



-- Normalising -----------------------------------------------------------------

type TrackingTaskT m a = WriterT (List (Someref m)) m (TaskT m a)

normalise ::
  Collaborative l m =>
  TaskT m a -> TrackingTaskT m a
normalise t = case t of
  -- * Step
  Step t1 e2 -> do
    t1' <- normalise t1
    let stay = Step t1' e2
    mv1 <- lift <| value t1'
    case mv1 of
      Nothing -> pure stay  -- S-ThenNone
      Just v1 ->
        let t2 = e2 v1 in
        if failing t2
          then pure stay -- S-ThenFail
          else if not <| null <| options t2
            then pure stay  -- S-ThenWait
            else normalise t2 -- S-ThenCont
  -- * Choose
  Choose t1 t2 -> do
    t1' <- normalise t1
    mv1 <- lift <| value t1'
    case mv1 of
      Just _  -> pure t1'  -- S-OrLeft
      Nothing -> do
        t2' <- normalise t2
        mv2 <- lift <| value t2'
        case mv2 of
          Just _  -> pure t2'  -- S-OrRight
          Nothing -> pure <| Choose t1' t2'  -- S-OrNone
  -- * Evaluate
  Trans f t1 -> pure (Trans f) -< normalise t1
  Forever t1 -> normalise <| Step t1 (\_ -> Forever t1)
  Pair t1 t2 -> pure Pair -< normalise t1 -< normalise t2
  -- * Internal
  Store v -> do
    l <- lift <| store v
    pure <| Done l
  Assign l v -> do
    lift <| l <<- v
    tell [ pack l ]
    pure <| Done ()
  -- * Ready
  Done _   -> pure t
  Enter    -> pure t
  Update _ -> pure t
  View _   -> pure t
  Watch _  -> pure t
  Change _ -> pure t
  Pick _   -> pure t
  Fail     -> pure t


balance :: TaskT m a -> TaskT m a
balance t = case t of
  Pair t1 t2   -> Pair (balance t1) (balance t2)
  Choose t1 t2 -> Choose (balance t1) (balance t2)
  Pick ts      -> Pick <| map balance ts
  Trans f t1   -> Trans f (balance t1)
  Forever t1   -> Forever (balance t1)

  -- Monad associativity
  Step (Step t1 e2) e3 -> Step t1 (\x -> Step (e2 x) e3)

  _ -> t


data Steps
  = DidStabilise Int Int
  | DidNotStabilise Int Int Int
  | DidNormalise Text

instance Pretty Steps where
  pretty = \case
    DidNotStabilise d w o -> sep [ "Found", pretty o, "overlap(s) amongst", pretty d, "dirty and", pretty w, "watched reference(s)" ]
    DidStabilise d w      -> sep [ "Found no overlaps amongst", pretty d, "dirty and", pretty w, "watched reference(s)" ]
    DidNormalise t        -> sep [ "Normalised to:", pretty t ]


stabilise ::
  Collaborative l m => MonadLog Steps m =>
  TaskT m a -> TrackingTaskT m a
--NOTE: This has *nothing* to do with iTasks Stable/Unstable values!
stabilise t = do
  ( t', ds ) <- listen <| normalise t
  log Info <| DidNormalise (show <| pretty t')
  let ws = watching t'
  let os = ds `intersect` ws
  case os of
    [] -> do
      log Info <| DidStabilise (length ds) (length ws)
      pure <| balance t'
    _  -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      stabilise t'


-- Handling --------------------------------------------------------------------


data NotApplicable
  = CouldNotChangeVal SomeTypeRep SomeTypeRep
  | CouldNotChangeRef SomeTypeRep SomeTypeRep
  | CouldNotGoTo Label
  | CouldNotFind Label
  | CouldNotPick
  | CouldNotContinue
  | CouldNotHandle (Input Action)


instance Pretty NotApplicable where
  pretty = \case
    CouldNotChangeVal v c -> sep [ "Could not change value because types", dquotes <| pretty v, "and", dquotes <| pretty c, "do not match" ]
    CouldNotChangeRef r c -> sep [ "Could not change value because cell", dquotes <| pretty r, "does not contain", dquotes <| pretty c ]
    CouldNotGoTo l        -> sep [ "Could not pick label", dquotes <| pretty l, "because it leads to an empty task" ]
    CouldNotFind l        -> sep [ "Could not find label", dquotes <| pretty l, "in the possible options" ]
    CouldNotPick          -> sep [ "Could not pick because there is nothing to pick from in this task" ]
    CouldNotContinue      -> sep [ "Could not continue because there is no value to continue with" ]
    CouldNotHandle i      -> sep [ "Could not handle input", dquotes <| pretty i, "(this should only appear when giving an impossible input)" ]


handle :: forall m l a.
  Collaborative l m => MonadLog NotApplicable m =>
  TaskT m a -> Input Action -> TrackingTaskT m a
handle t i = case ( t, i ) of
  -- * Edit
  ( Enter, ToHere (IChange w) )
    | Just Refl <- r ~~ typeOf w -> pure <| Update w
    | otherwise -> do
        log Warning <| CouldNotChangeVal (SomeTypeRep r) (someTypeOf w)
        pure t
    where
      r = typeRep :: TypeRep a
  ( Update v, ToHere (IChange w) )
    -- NOTE: Here we check if `v` and `w` have the same type.
    -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
    | Just Refl <- v ~= w -> pure <| Update w
    | otherwise -> do
        log Warning <| CouldNotChangeVal (someTypeOf v) (someTypeOf w)
        pure t
  ( Change l, ToHere (IChange w) )
    -- NOTE: As in the `Update` case above, we check for type equality.
    | Just Refl <- r ~~ typeOf w -> do
        l <<- w
        tell [ pack l ]
        pure <| Change l
    | otherwise -> do
        log Warning <| CouldNotChangeRef (someTypeOf l) (someTypeOf w)
        pure t
    where
      r = typeRep :: TypeRep a
  -- * Pick
  ( Pick _, ToHere (IPick l) ) ->
    handlePick l t
  ( Step t1 e2, ToHere (IContinue l) ) -> do
    mv1 <- lift <| value t1
    case mv1 of
      Nothing -> do
        log Warning CouldNotContinue
        pure t
      Just v1 -> handlePick l (e2 v1)
  -- * Passing
  ( Trans f t1, i' ) -> do
    t1' <- handle t1 i'
    pure <| Trans f t1'
  ( Forever t1, i' ) -> do
    t1' <- handle t1 i'
    pure <| Forever t1'
  ( Step t1 t2, i' ) -> do
    t1' <- handle t1 i'
    pure <| Step t1' t2
  ( Pair t1 t2, ToFirst i' ) -> do
    t1' <- handle t1 i'
    pure <| Pair t1' t2
  ( Pair t1 t2, ToSecond i' ) -> do
    t2' <- handle t2 i'
    pure <| Pair t1 t2'
  ( Choose t1 t2, ToFirst i' ) -> do
    t1' <- handle t1 i'
    pure <| Choose t1' t2
  ( Choose t1 t2, ToSecond i' ) -> do
    t2' <- handle t2 i'
    pure <| Choose t1 t2'
  -- * Rest
  _ -> do
    log Error <| CouldNotHandle i
    pure t
  where
    handlePick l c = case c of
      Pick ts -> case Dict.lookup l ts of
        Nothing -> do
          log Warning <| CouldNotFind l
          pure t
        Just t' -> if Set.member l (options c)
          then pure t'
          else do
            log Warning <| CouldNotGoTo l
            pure t
      _ -> do
        log Warning <| CouldNotPick
        pure t




initialise ::
  Collaborative l m => MonadLog Steps m =>
  TaskT m a -> m (TaskT m a)
initialise = evalWriterT << stabilise


interact ::
  Collaborative l m => MonadLog NotApplicable m => MonadLog Steps m =>
  TaskT m a -> Input Action -> m (TaskT m a)
interact t i = evalWriterT (handle t i >>= stabilise)



-- Running ---------------------------------------------------------------------


getUserInput :: IO (Input Action)
getUserInput = do
  putText ">> "
  line <- getLine
  case line of
    "quit" -> exitSuccess
    _ -> case parse (words line) of
      Right input -> pure input
      Left message -> do
        print message
        getUserInput


loop :: Task a -> IO ()
loop task = do
  putTextLn ""
  interface <- ui task
  print interface
  events <- inputs task
  print <| "Possibilities: " <> pretty events
  input <- getUserInput
  task' <- interact task input
  loop task'


run :: Task a -> IO ()
run task = do
  task' <- initialise task
  loop task'
