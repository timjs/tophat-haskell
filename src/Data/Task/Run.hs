module Data.Task.Run where

import Control.Monad.Log
import Control.Monad.Supply
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (intersect, union)
import Data.Task
import Data.Task.Input

-- Observations ----------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.

ui ::
  Collaborative r m =>
  Task m a ->
  m (Doc b) -- We need to watch locations and be in monad `m`
ui = \case
  New e -> pure (\s -> sep ["ν", s]) -< ui' e
  Editor _ e -> ui' e
  Done _ -> pure "■(…)"
  Pair t1 t2 -> pure (\l r -> sep [l, " ⧓ ", r]) -< ui t1 -< ui t2
  Choose t1 t2 -> pure (\l r -> sep [l, " ◆ ", r]) -< ui t1 -< ui t2
  Fail -> pure "↯"
  Trans _ t -> ui t
  Forever t -> pure (\s -> cat ["⟲", s]) -< ui t
  Step t1 e2 -> pure (\s n -> cat [s, " ▶", pretty n, "…"]) -< ui t1 -< count
    where
      count = do
        mv1 <- value t1
        case mv1 of
          Nothing -> pure 0
          Just v1 -> pure <| length <| picks (e2 v1)
  Share v -> pure <| sep ["share", pretty v]
  Assign v _ -> pure <| sep ["…", ":=", pretty v]

ui' ::
  Collaborative r m =>
  Editor m a ->
  m (Doc b) -- We need to watch locations and be in monad `m`
ui' = \case
  Enter -> pure "⊠()"
  Update v -> pure <| cat ["□(", pretty v, ")"]
  View v -> pure <| cat ["⧇(", pretty v, ")"]
  Select ts -> pure <| cat ["◇", pretty <| HashMap.keysSet ts]
  Change l -> pure (\v -> cat ["⊟(", pretty v, ")"]) -< watch l
  Watch l -> pure (\v -> cat ["⧈(", pretty v, ")"]) -< watch l

value ::
  Collaborative r m =>
  Task m a ->
  m (Maybe a) -- We need to watch locations and be in monad `m`
value = \case
  New _ -> pure Nothing
  Editor _ e -> value' e
  Done v -> pure (Just v)
  Pair t1 t2 -> pure (><) -< value t1 -< value t2
  Choose t1 t2 -> pure (<|>) -< value t1 -< value t2
  Fail -> pure Nothing
  Trans f t -> pure (map f) -< value t
  Forever _ -> pure Nothing
  Step _ _ -> pure Nothing
  Share v -> pure Just -< share v -- Nothing???
  Assign _ _ -> pure (Just ())

value' ::
  Collaborative r m =>
  Editor m a ->
  m (Maybe a) -- We need to watch locations and be in monad `m`
value' = \case
  Enter -> pure Nothing
  Update v -> pure (Just v)
  View v -> pure (Just v)
  Select _ -> pure Nothing
  Change l -> pure Just -< watch l
  Watch l -> pure Just -< watch l

failing ::
  Task m a -> Bool
failing = \case
  New e -> failing' e
  Editor _ e -> failing' e
  Done _ -> False
  Pair t1 t2 -> failing t1 && failing t2
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Trans _ t -> failing t
  Forever t -> failing t
  Step t _ -> failing t
  Share _ -> False
  Assign _ _ -> False

failing' ::
  Editor m a -> Bool
failing' = \case
  Enter -> False
  Update _ -> False
  View _ -> False
  Select ts -> all failing ts
  Change _ -> False
  Watch _ -> False

watching ::
  Collaborative r m => -- We need Collaborative here to pack references `r`
  Task m a ->
  List (Someref m) -- But there is no need to be in monad `m`
watching = \case
  New _ -> []
  Editor _ e -> watching' e
  Done _ -> []
  Pair t1 t2 -> watching t1 `union` watching t2
  Choose t1 t2 -> watching t1 `union` watching t2
  Fail -> []
  Trans _ t2 -> watching t2
  Forever t1 -> watching t1
  Step t1 _ -> watching t1
  Share _ -> []
  Assign _ _ -> []

watching' ::
  -- Collaborative r m => -- We need Collaborative here to pack references `r`
  Editor m a ->
  List (Someref m) -- But there is no need to be in monad `m`
watching' = \case
  Enter -> []
  Update _ -> []
  View _ -> []
  Select _ -> []
  Change (Store _ r) -> [pack r]
  Watch (Store _ r) -> [pack r]

picks ::
  Task m a -> HashSet Label
picks = \case
  Editor _ e -> picks' e
  Trans _ t2 -> picks t2
  Forever t1 -> picks t1
  Step t1 _ -> picks t1
  _ -> []

picks' ::
  Editor m a -> HashSet Label
picks' = \case
  Select ts -> HashMap.keysSet <| HashMap.filter (not << failing) ts
  _ -> []

inputs ::
  forall m r a.
  Collaborative r m =>
  Task m a ->
  m (List (Input Dummy)) -- We need to call `value`, therefore we are in `m`
inputs t = case t of
  New _ -> pure []
  Editor _ e -> inputs' e
  Done _ -> pure []
  Pair t1 t2 -> pure (\l r -> map ToFirst l ++ map ToSecond r) -< inputs t1 -< inputs t2
  Choose t1 t2 -> pure (\l r -> map ToFirst l ++ map ToSecond r) -< inputs t1 -< inputs t2
  Fail -> pure []
  Trans _ t2 -> inputs t2
  Forever t1 -> inputs t1
  Step t1 e2 -> pure (++) -< inputs t1 -< do
    mv1 <- value t1
    case mv1 of
      Nothing -> pure []
      Just v1 -> do
        -- t2 <- normalise (e2 v1)  --XXX: Do we need to normalise here??
        let t2 = e2 v1
        if failing t2
          then pure []
          else pure <| map (ToHere << AContinue) <| HashSet.toList <| picks t2
  Share _ -> pure []
  Assign _ _ -> pure []

inputs' ::
  forall m r a.
  Collaborative r m =>
  Editor m a ->
  m (List (Input Dummy)) -- We need to call `value`, therefore we are in `m`
inputs' t = case t of
  Enter -> pure [ToHere (AEnter tau)]
  Update _ -> pure [ToHere (AEnter tau)]
  View _ -> pure []
  Select _ -> pure <| map (ToHere << ASelect) <| HashSet.toList <| picks' t
  Change _ -> pure [ToHere (AEnter tau)]
  Watch _ -> pure []
  where
    tau = Proxy :: Proxy a

-- Normalising -----------------------------------------------------------------

type Tracking f m a = WriterT (List (Someref m)) m (f m a)

normalise ::
  Collaborative r m =>
  MonadSupply Unique m =>
  Task m a ->
  Tracking Task m a
normalise t = case t of
  -- Step --
  Step t1 e2 -> do
    t1' <- normalise t1
    let stay = Step t1' e2
    mv1 <- lift <| value t1'
    case mv1 of
      Nothing -> pure stay -- S-ThenNone
      Just v1 ->
        let t2 = e2 v1
         in if failing t2
              then pure stay -- S-ThenFail
              else
                if not <| null <| picks t2
                  then pure stay -- S-ThenWait
                  else normalise t2 -- S-ThenCont
                      -- Choose --
  Choose t1 t2 -> do
    t1' <- normalise t1
    mv1 <- lift <| value t1'
    case mv1 of
      Just _ -> pure t1' -- S-OrLeft
      Nothing -> do
        t2' <- normalise t2
        mv2 <- lift <| value t2'
        case mv2 of
          Just _ -> pure t2' -- S-OrRight
          Nothing -> pure <| Choose t1' t2' -- S-OrNone
                -- Evaluate --
  Trans f t1 -> pure (Trans f) -< normalise t1
  Forever t1 -> normalise <| Step t1 (\_ -> Forever t1)
  Pair t1 t2 -> pure Pair -< normalise t1 -< normalise t2
  -- Internal --
  Share v -> do
    l <- lift <| share v
    pure <| Done l
  Assign v s@(Store _ r) -> do
    lift <| s <<- v
    tell [pack r]
    pure <| Done ()
  -- Ready --
  New e -> do
    n <- supply
    pure <| Editor n e
  Editor _ _ -> pure t
  Done _ -> pure t
  Fail -> pure t

balance :: Task m a -> Task m a
balance t = case t of
  Pair t1 t2 -> Pair (balance t1) (balance t2)
  Choose t1 t2 -> Choose (balance t1) (balance t2)
  Trans f t1 -> Trans f (balance t1)
  Forever t1 -> Forever (balance t1)
  -- Monad associativity
  Step (Step t1 e2) e3 -> Step t1 (\x -> Step (e2 x) e3)
  _ -> t

balance' :: Editor m a -> Editor m a
balance' = \case
  Select ts -> Select <| map balance ts
  e -> e

-- Handling --------------------------------------------------------------------

type PartialTracking f m a = WriterT (List (Someref m)) (ExceptT NotApplicable m) (f m a)

-- type PartialTrackingTask m a = ExceptT NotApplicable (WriterT (List (Someref m)) m) (Task m a)

data NotApplicable
  = CouldNotChangeVal SomeTypeRep SomeTypeRep
  | CouldNotChangeRef SomeTypeRep SomeTypeRep
  | CouldNotGoTo Label
  | CouldNotFind Label
  | CouldNotSelect
  | CouldNotContinue
  | CouldNotHandle (Input Action)

instance Pretty NotApplicable where
  pretty = \case
    CouldNotChangeVal v c -> sep ["Could not change value because types", dquotes <| pretty v, "and", dquotes <| pretty c, "do not match"]
    CouldNotChangeRef r c -> sep ["Could not change value because cell", dquotes <| pretty r, "does not contain", dquotes <| pretty c]
    CouldNotGoTo l -> sep ["Could not pick label", dquotes <| pretty l, "because it leads to an empty task"]
    CouldNotFind l -> sep ["Could not find label", dquotes <| pretty l, "in the possible picks"]
    CouldNotSelect -> sep ["Could not pick because there is nothing to pick from in this task"]
    CouldNotContinue -> sep ["Could not continue because there is no value to continue with"]
    CouldNotHandle i -> sep ["Could not handle input", dquotes <| pretty i, "(this should only appear when giving an impossible input)"]

handle ::
  forall m r a.
  Collaborative r m =>
  Task m a ->
  Input Action ->
  PartialTracking Task m a
handle t i = case (t, i) of
  -- Editors --
  (Editor _ (Select ts), ToHere (ISelect l)) -> case HashMap.lookup l ts of
    Nothing -> throw <| CouldNotFind l
    Just t' ->
      if l =< (picks t)
        then pure t'
        else throw <| CouldNotGoTo l
  (Editor n e, _) -> do
    e' <- handle' e i
    pure <| Editor n e'
  -- Step --
  (Step t1 e2, ToHere (IContinue l)) -> do
    mv1 <- lift <| lift <| value t1
    case mv1 of
      Nothing -> throw CouldNotContinue
      Just v1 -> handle (e2 v1) (ToHere (ISelect l))
  -- Pass --
  (Trans f t1, i') -> do
    t1' <- handle t1 i'
    pure <| Trans f t1'
  (Forever t1, i') -> do
    t1' <- handle t1 i'
    pure <| Step t1' (\_ -> Forever t1)
  -- OR: handle (Step t1 (\_ -> Forever t1)) i'
  (Step t1 e2, i') -> do
    t1' <- handle t1 i'
    pure <| Step t1' e2
  (Pair t1 t2, ToFirst i') -> do
    t1' <- handle t1 i'
    pure <| Pair t1' t2
  (Pair t1 t2, ToSecond i') -> do
    t2' <- handle t2 i'
    pure <| Pair t1 t2'
  (Choose t1 t2, ToFirst i') -> do
    t1' <- handle t1 i'
    pure <| Choose t1' t2
  (Choose t1 t2, ToSecond i') -> do
    t2' <- handle t2 i'
    pure <| Choose t1 t2'
  -- Rest --
  _ -> throw <| CouldNotHandle i

handle' ::
  forall m r a.
  Collaborative r m =>
  Editor m a -> -- `Select` does not return an `Editor`...
  Input Action ->
  PartialTracking Editor m a
handle' e i = case (e, i) of
  (Enter, ToHere (IEnter w))
    | Just Refl <- tau ~? typeOf w -> pure <| Update w
    | otherwise -> throw <| CouldNotChangeVal (SomeTypeRep tau) (someTypeOf w)
    where
      tau = typeRep :: TypeRep a
  (Update v, ToHere (IEnter w))
    -- NOTE: Here we check if `v` and `w` have the same type.
    -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
    | Just Refl <- v ~= w -> pure <| Update w
    | otherwise -> throw <| CouldNotChangeVal (someTypeOf v) (someTypeOf w)
  (Change s@(Store _ r), ToHere (IEnter w))
    -- NOTE: As in the `Update` case above, we check for type equality.
    | Just Refl <- tau ~? typeOf w -> do
      s <<- w
      tell [pack r]
      pure <| Change s
    | otherwise -> throw <| CouldNotChangeRef (someTypeOf s) (someTypeOf w)
    where
      tau = typeRep :: TypeRep a
  -- Rest --
  _ -> throw <| CouldNotHandle i

-- Interaction -----------------------------------------------------------------

data Steps
  = DidStabilise Nat Nat
  | DidNotStabilise Nat Nat Nat
  | DidNormalise Text
  | DidBalance Text

instance Pretty Steps where
  pretty = \case
    DidNotStabilise d w o -> sep ["Found", pretty o, "overlap(s) amongst", pretty d, "dirty and", pretty w, "watched reference(s)"]
    DidStabilise d w -> sep ["Found no overlaps amongst", pretty d, "dirty and", pretty w, "watched reference(s)"]
    DidNormalise t -> sep ["Normalised to:", pretty t]
    DidBalance t -> sep ["Rebalanced to:", pretty t]

fixate ::
  Collaborative r m =>
  MonadSupply Unique m =>
  MonadLog Steps m =>
  Tracking Task m a ->
  m (Task m a)
fixate tt = do
  (t, d) <- runWriterT tt
  (t', d') <- runWriterT <| normalise t
  log Info <| DidNormalise (show <| pretty t')
  let ws = watching t'
  let ds = d `union` d'
  let os = ds `intersect` ws
  case os of
    [] -> do
      let t'' = balance t'
      log Info <| DidStabilise (length ds) (length ws)
      log Info <| DidBalance (show <| pretty t'')
      pure t''
    _ -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      fixate <| pure t'

initialise ::
  Collaborative r m =>
  MonadSupply Unique m =>
  MonadLog Steps m =>
  Task m a ->
  m (Task m a)
initialise = fixate << pure

interact ::
  Collaborative r m =>
  MonadSupply Unique m =>
  MonadLog NotApplicable m =>
  MonadLog Steps m =>
  Task m a ->
  Input Action ->
  m (Task m a)
interact t i = do
  xt <- runExceptT <| runWriterT <| handle t i
  case xt of
    Left e -> do
      log Warning e
      pure t
    Right tt -> fixate <| WriterT <| pure tt

execute ::
  Editable a =>
  List (Input Action) ->
  Task IO a ->
  IO ()
execute events task = initialise task >>= go events
  where
    go events' task' = case events' of
      event : rest -> do
        task'' <- interact task' event
        go rest task''
      [] -> do
        result <- value task'
        putTextLn <| show <| pretty result

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

run :: Task IO a -> IO ()
run task = do
  task' <- initialise task
  loop task'
  where
    loop :: Task IO a -> IO ()
    loop task' = do
      putTextLn ""
      interface <- ui task'
      print interface
      events <- inputs task'
      print <| "Possibilities: " ++ pretty events
      input <- getUserInput
      task'' <- interact task' input
      loop task''
