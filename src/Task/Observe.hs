module Task.Observe where

import Data.List (union)
import qualified Data.Store as Store
import Polysemy
import Polysemy.Mutate (Alloc, Read)
import Task.Input
import Task.Syntax

---- Observations --------------------------------------------------------------
-- NOTE: Normalisation should never happen in any observation, they are immediate.

-- FIXME: should only be defined on NormalTask's
ui ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` because `Value` needs it.
  Task h a ->
  Sem r Text
ui = \case
  Edit n e -> ui' n e
  Select n t1 cs -> done (\l -> concat [l, " ▷^", display n, " ", display <| keys cs]) -<< ui t1
  Pool n t0 ts -> done (\is -> unwords ["⋈^", display n, display is]) -<< traverse ui ts
  Lift _ -> done "■ …"
  Pair t1 t2 -> done (\l r -> unwords [l, " ⧓ ", r]) -<< ui t1 -<< ui t2
  Choose t1 t2 -> done (\l r -> unwords [l, " ◆ ", r]) -<< ui t1 -<< ui t2
  Fail -> done "↯"
  Trans _ t -> ui t
  Step t1 _ -> done (\l -> concat [l, " ▶…"]) -<< ui t1
  Assert _ -> done <| unwords ["assert", "…"]
  Share b -> done <| unwords ["share", display b]
  Assign b _ -> done <| unwords ["…", ":=", display b]

ui' ::
  (Members '[Read h] r) => -- We need `Read` to read from references.
  Name ->
  Editor h b ->
  Sem r Text
ui' n = \case
  Enter -> done <| concat ["□^", display n, " [ ] "]
  Update b -> done <| concat ["⊟^", display n, " [ ", display b, " ]"]
  View b -> done <| concat ["⧇^", display n, " [ ", display b, " ]"]
  Change l -> done (\b -> concat ["^⊞", display n, " [ ", display b, " ]"]) -<< Store.read l
  Watch l -> done (\b -> concat ["⧈^", display n, " [ ", display b, " ]"]) -<< Store.read l

-- FIXME: should only be defined on NormalTask's
value ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` to allocate a fresh store to continue with.
  Task h a ->
  Sem r (Maybe a)
value = \case
  Edit Unnamed _ -> done Nothing
  Edit (Named _) e -> value' e
  Select _ _ _ -> done Nothing
  Pool _ _ ts -> sequence <-< traverse value ts
  Trans f t -> done (map f) -<< value t
  Pair t1 t2 -> done (<&>) -<< value t1 -<< value t2
  Lift v -> done (Just v)
  Choose t1 t2 -> done (<|>) -<< value t1 -<< value t2
  Fail -> done Nothing
  Step _ _ -> done Nothing
  Assert b -> done (Just b)
  Share b -> done Just -<< Store.alloc b
  Assign _ _ -> done (Just ())

value' ::
  (Members '[Read h] r) => -- We need `Read` to read from references.
  Editor h a ->
  Sem r (Maybe a)
value' = \case
  Enter -> done Nothing
  Update b -> done (Just b)
  View b -> done (Just b)
  Change l -> done Just -<< Store.read l
  Watch l -> done Just -<< Store.read l

failing ::
  Task h a ->
  Bool
failing = \case
  Edit _ _ -> False
  Select _ t1 _ -> failing t1
  Pool _ _ _ -> False -- Always able to start a new task
  Trans _ t2 -> failing t2
  Pair t1 t2 -> failing t1 && failing t2
  Lift _ -> False
  Choose t1 t2 -> failing t1 && failing t2
  Fail -> True
  Step t1 _ -> failing t1
  Assert _ -> False
  Share _ -> False
  Assign _ _ -> False

-- FIXME: should only be defined on NormalTask's
watching ::
  Task h a ->
  List (Some (Ref h)) -- There is no need for any effects.
watching = \case
  Edit Unnamed _ -> []
  Edit (Named _) e -> watching' e
  Select _ t1 _ -> watching t1
  Pool _ _ ts -> ts |> map watching |> foldr union []
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

-- FIXME: should only be defined on NormalTask's
inputs ::
  (Members '[Alloc h, Read h] r) => -- We need `Alloc` and `Read` because we call `value`.
  Task h a ->
  Sem r (List (Input Abstract))
inputs t = case t of
  Edit Unnamed _ -> done []
  Edit (Named k) e -> done <| map (Send k) (inputs' e)
  Select Unnamed _ _ -> done []
  Select (Named k) t1 ts ->
    done (++) -<< inputs t1 -<< do
      mv1 <- value t1
      case mv1 of
        Nothing -> done []
        Just v1 -> done [Send k (Decide l) | (l, e) <- ts, not <| failing (e v1)]
  Pool Unnamed _ _ -> done []
  Pool (Named k) _ ts -> do
    let l = length ts
    let is = map (Send k) ([Start] ++ [Duplicate i | i <- [1 .. l]] ++ [Terminate i | i <- [1 .. l]])
    is' <- concat <-< traverse inputs ts
    done <| is ++ is'
  Trans _ t2 -> inputs t2
  Pair t1 t2 -> done (++) -<< inputs t1 -<< inputs t2
  Lift _ -> done []
  Choose t1 t2 -> done (++) -<< inputs t1 -<< inputs t2
  Fail -> done []
  Step t1 _ -> inputs t1
  Assert _ -> done []
  Share _ -> done []
  Assign _ _ -> done []

inputs' ::
  forall h a.
  Editor h a ->
  List (Action Abstract)
inputs' t = case t of
  Enter -> [Insert <| Abstract beta]
  Update _ -> [Insert <| Abstract beta]
  View _ -> []
  Change _ -> [Insert <| Abstract beta]
  Watch _ -> []
  where
    beta = Proxy :: Proxy a
