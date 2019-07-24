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
          Just v1 -> pure <| length <| picks (e2 v1)

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


picks ::
  TaskT m a -> Set Label
picks = \case
  Pick ts    -> Dict.keysSet <| Dict.filter (not << failing) ts

  Trans _ t1 -> picks t1
  Forever t1 -> picks t1
  Step t1 _  -> picks t1

  _ -> []


inputs :: forall m l a.
  Collaborative l m =>
  TaskT m a -> m (List (Input Dummy))  -- We need to call `value`, therefore we are in `m`
inputs t = case t of
  Done _       -> pure []
  Enter        -> pure [ ToHere (AEnter tau) ]
  Update _     -> pure [ ToHere (AEnter tau) ]
  View _       -> pure []

  Pair t1 t2   -> pure (\l r -> map ToFirst l <> map ToSecond r) -< inputs t1 -< inputs t2
  Choose t1 t2 -> pure (\l r -> map ToFirst l <> map ToSecond r) -< inputs t1 -< inputs t2
  Pick _       -> pure <| map (ToHere << APick) <| Set.toList <| picks t
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
          else pure <| map (ToHere << AContinue) <| Set.toList <| picks t2

  Store _      -> pure []
  Assign _ _   -> pure []
  Watch _      -> pure []
  Change _     -> pure [ ToHere (AEnter tau) ]
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
          else if not <| null <| picks t2
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
  | DidBalance Text

instance Pretty Steps where
  pretty = \case
    DidNotStabilise d w o -> sep [ "Found", pretty o, "overlap(s) amongst", pretty d, "dirty and", pretty w, "watched reference(s)" ]
    DidStabilise d w      -> sep [ "Found no overlaps amongst", pretty d, "dirty and", pretty w, "watched reference(s)" ]
    DidNormalise t        -> sep [ "Normalised to:", pretty t ]
    DidBalance t          -> sep [ "Rebalanced to:", pretty t ]


stabilise ::
  Collaborative l m => MonadLog Steps m =>
  TrackingTaskT m a -> m (TaskT m a)
--NOTE: This has *nothing* to do with iTasks Stable/Unstable values!
stabilise tt = do
  ( t, d ) <- runWriterT tt
  ( t', d' ) <- runWriterT <| normalise t
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
    _  -> do
      log Info <| DidNotStabilise (length ds) (length ws) (length os)
      stabilise <| pure t'


-- Handling --------------------------------------------------------------------

type PartialTrackingTaskT m a = WriterT (List (Someref m)) (ExceptT NotApplicable  m) (TaskT m a)
-- type PartialTrackingTaskT m a = ExceptT NotApplicable (WriterT (List (Someref m)) m) (TaskT m a)

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
    CouldNotFind l        -> sep [ "Could not find label", dquotes <| pretty l, "in the possible picks" ]
    CouldNotPick          -> sep [ "Could not pick because there is nothing to pick from in this task" ]
    CouldNotContinue      -> sep [ "Could not continue because there is no value to continue with" ]
    CouldNotHandle i      -> sep [ "Could not handle input", dquotes <| pretty i, "(this should only appear when giving an impossible input)" ]


handle :: forall m l a.
  Collaborative l m =>
  TaskT m a -> Input Action -> PartialTrackingTaskT m a
handle t i = case ( t, i ) of
  -- * Edit
  ( Enter, ToHere (IEnter w) )
    | Just Refl <- r ~~ typeOf w -> pure <| Update w
    | otherwise -> throw <| CouldNotChangeVal (SomeTypeRep r) (someTypeOf w)
    where
      r = typeRep :: TypeRep a
  ( Update v, ToHere (IEnter w) )
    -- NOTE: Here we check if `v` and `w` have the same type.
    -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
    | Just Refl <- v ~= w -> pure <| Update w
    | otherwise -> throw <| CouldNotChangeVal (someTypeOf v) (someTypeOf w)
  ( Change l, ToHere (IEnter w) )
    -- NOTE: As in the `Update` case above, we check for type equality.
    | Just Refl <- r ~~ typeOf w -> do
        l <<- w
        tell [ pack l ]
        pure <| Change l
    | otherwise -> throw <| CouldNotChangeRef (someTypeOf l) (someTypeOf w)
    where
      r = typeRep :: TypeRep a
  -- * Pick
  ( Pick ts, ToHere (IPick l) ) -> case Dict.lookup l ts of
      Nothing -> throw <| CouldNotFind l
      Just t' -> if Set.member l (picks t)
        then pure t'
        else throw <| CouldNotGoTo l
  ( Step t1 e2, ToHere (IContinue l) ) -> do
    mv1 <- lift <| lift <| value t1
    case mv1 of
      Nothing -> throw CouldNotContinue
      Just v1 -> handle (e2 v1) (ToHere (IPick l))
  -- * Passing
  ( Trans f t1, i' ) -> do
    t1' <- handle t1 i'
    pure <| Trans f t1'
  ( Forever t1, i' ) -> do
    t1' <- handle t1 i'
    pure <| Step t1' (\_ -> Forever t1)
    -- OR: handle (Step t1 (\_ -> Forever t1)) i'
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
  _ -> throw <| CouldNotHandle i


initialise ::
  Collaborative l m => MonadLog Steps m =>
  TaskT m a -> m (TaskT m a)
initialise = stabilise << pure


interact ::
  Collaborative l m => MonadLog NotApplicable m => MonadLog Steps m =>
  TaskT m a -> Input Action -> m (TaskT m a)
interact t i = do
  xt <- runExceptT <| runWriterT <| handle t i
  case xt of
    Left e -> do
        log Warning e
        pure t
    Right tt -> stabilise <| WriterT <| pure tt



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
