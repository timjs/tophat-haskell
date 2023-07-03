module Task.Observe where

import Data.List (union)
import qualified Data.Store as Store
import Polysemy
import Polysemy.Mutate (Alloc, Read)
import Task.Input
import Task.Syntax

---- Observations --------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.

ui ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` because `Value` needs it.
  Task h a ->
  Sem r Text
ui = \case
  Edit n e -> ui' n e
  Select n t1 ts -> pure (\l -> concat [l, " ▷^", display n, " ", display <| keys ts]) -< ui t1
  Lift _ -> pure "■ …"
  Pair t1 t2 -> pure (\l r -> unwords [l, " ⧓ ", r]) -< ui t1 -< ui t2
  Choose t1 t2 -> pure (\l r -> unwords [l, " ◆ ", r]) -< ui t1 -< ui t2
  Fail -> pure "↯"
  Trans _ t -> ui t
  Step t1 _ -> pure (\l -> concat [l, " ▶…"]) -< ui t1
  Assert _ -> pure <| unwords ["assert", "…"]
  Share b -> pure <| unwords ["share", display b]
  Assign b _ -> pure <| unwords ["…", ":=", display b]

ui' ::
  (Members '[Read h] r) => -- We need `Read` to read from references.
  Name ->
  Editor h b ->
  Sem r Text
ui' n = \case
  Enter -> pure <| concat ["□^", display n, " [ ] "]
  Update b -> pure <| concat ["⊟^", display n, " [ ", display b, " ]"]
  View b -> pure <| concat ["⧇^", display n, " [ ", display b, " ]"]
  Change l -> pure (\b -> concat ["^⊞", display n, " [ ", display b, " ]"]) -< Store.read l
  Watch l -> pure (\b -> concat ["⧈^", display n, " [ ", display b, " ]"]) -< Store.read l

value ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` to allocate a fresh store to continue with.
  Task h a ->
  Sem r (Maybe a)
value = \case
  Edit Unnamed _ -> pure Nothing
  Edit (Named _) e -> value' e
  Select _ _ _ -> pure Nothing
  Trans f t -> pure (map f) -< value t
  Pair t1 t2 -> pure (><) -< value t1 -< value t2
  Lift v -> pure (Just v)
  Choose t1 t2 -> pure (<|>) -< value t1 -< value t2
  Fail -> pure Nothing
  Step _ _ -> pure Nothing
  Assert b -> pure (Just b)
  Share b -> pure Just -< Store.alloc b
  Assign _ _ -> pure (Just ())

value' ::
  (Members '[Read h] r) => -- We need `Read` to read from references.
  Editor h a ->
  Sem r (Maybe a)
value' = \case
  Enter -> pure Nothing
  Update b -> pure (Just b)
  View b -> pure (Just b)
  Change l -> pure Just -< Store.read l
  Watch l -> pure Just -< Store.read l

failing ::
  Task h a ->
  Bool
failing = \case
  Edit _ _ -> False
  Select _ t1 _ -> failing t1
  Trans _ t2 -> failing t2
  Pair t1 t2 -> failing t1 && failing t2
  Lift _ -> False
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Step t1 _ -> failing t1
  Assert _ -> False
  Share _ -> False
  Assign _ _ -> False

watching ::
  Task h a ->
  List (Some (Ref h)) -- There is no need for any effects.
watching = \case
  Edit Unnamed _ -> []
  Edit (Named _) e -> watching' e
  Select _ t1 _ -> watching t1
  Trans _ t2 -> watching t2
  Pair t1 t2 -> watching t1 `union` watching t2
  Lift _ -> []
  Choose t1 t2 -> watching t1 `union` watching t2
  Fail -> []
  Step t1 _ -> watching t1
  Assert _ -> []
  Share _ -> []
  Assign _ _ -> []

watching' ::
  Editor h a ->
  List (Some (Ref h)) -- There is no need for any effects.
watching' = \case
  Enter -> []
  Update _ -> []
  View _ -> []
  Change (Store _ r) -> [pack r]
  Watch (Store _ r) -> [pack r]

inputs ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` and `Read` because we call `value`.
  Task h a ->
  Sem r (List (Input Abstract))
inputs t = case t of
  Edit Unnamed _ -> pure []
  Edit (Named k) e -> pure <| inputs' k e
  Select Unnamed _ _ -> pure []
  Select (Named k) t1 ts ->
    pure (++) -< inputs t1 -< do
      mv1 <- value t1
      case mv1 of
        Nothing -> pure []
        Just v1 -> pure [Decide k l | (l, e) <- ts, not <| failing (e v1)]
  Trans _ t2 -> inputs t2
  Pair t1 t2 -> pure (++) -< inputs t1 -< inputs t2
  Lift _ -> pure []
  Choose t1 t2 -> pure (++) -< inputs t1 -< inputs t2
  Fail -> pure []
  Step t1 _ -> inputs t1
  Assert _ -> pure []
  Share _ -> pure []
  Assign _ _ -> pure []

inputs' ::
  forall h a.
  Nat ->
  Editor h a ->
  List (Input Abstract)
inputs' k t = case t of
  Enter -> [Insert k <| Abstract beta]
  Update _ -> [Insert k <| Abstract beta]
  View _ -> []
  Change _ -> [Insert k <| Abstract beta]
  Watch _ -> []
  where
    beta = Proxy :: Proxy a
