module Task.Internal


import Control.Monad.Ref

import Task.Universe
import Task.Event


%default total
%access export

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe


infix  6 =~, /~



-- Tasks -----------------------------------------------------------------------


public export
data TaskT : (m : Type -> Type) -> Ty -> Type where

  -- Core
  Edit  : (val : Maybe (typeOf a)) -> TaskT m a
  Watch : MonadRef l m => l (typeOf b) -> TaskT m (PRIM b)

  -- Parallel
  All   : Show (typeOf a) => Show (typeOf b) => (left : TaskT m a) -> (right : TaskT m b) -> TaskT m (PAIR a b)

  -- Choice
  Any   : Show (typeOf a) => (left : TaskT m a) -> (right : TaskT m a) -> TaskT m a
  One   : Show (typeOf a) => (left : TaskT m a) -> (right : TaskT m a) -> TaskT m a
  Fail  : TaskT m a

  -- Sequence
  Then  : Show (typeOf a) => (this : TaskT m a) -> (next : typeOf a -> TaskT m b) -> TaskT m b
  Next  : Show (typeOf a) => (this : TaskT m a) -> (next : typeOf a -> TaskT m b) -> TaskT m b

  -- Labels
  Label : Label -> (this : TaskT m a) -> TaskT m a

  -- Lift
  Lift : Monad m => m (typeOf a) -> TaskT m a



-- Labels ----------------------------------------------------------------------


||| Get the current label, if one
label : TaskT m a -> Maybe Label
label (Label l _) = Just l
label _           = Nothing


||| Remove as much labels as possible from a task.
|||
||| Usefull to deeply match task constructors while ignoring labels.
delabel : TaskT m a -> TaskT m a
delabel (Label _ t) = delabel t
delabel t           = t


||| Match a label to a task.
(=~) : Label -> TaskT m a -> Bool
(=~) k (Label l _) = l == l
(=~) _ _           = False


||| Negation of `(=~)`.
(/~) : Label -> TaskT m a -> Bool
(/~) l t = not (l =~ t)


||| Collect all labels in an external choice
labels : TaskT m a -> List Label
labels (Label _ Fail)   = []
labels (Label l this)   = l :: labels this
labels (One left right) = labels left ++ labels right
-- --FIXME: should we also check for labels on the lhs of a step (see also `find`)?
-- labels (Then this _)    = labels this
-- labels (Next this _)    = labels this
labels _                = []


||| Depth first search for a label on a task tree.
|||
||| Returns the path of the found task.
find : Label -> TaskT m a -> Maybe Path
find k (Label l this) with ( k == l )
  | True                = Just GoHere
  | False               = find k this
find k (One left right) = map GoLeft (find k left) <|> map GoRight (find k right)
-- --FIXME: should we can send pick-events through to the lhs of a step (see also `labels`)?
-- find k (Then this _)    = find k this
-- find k (Next this _)    = find k this
find k _                = Nothing


||| Check if a task constructor keeps its label after stepping or loses it.
keeper : TaskT m a -> Bool
keeper (Edit _)  = True
keeper (All _ _) = True
keeper (Fail)    = True
keeper _         = False
