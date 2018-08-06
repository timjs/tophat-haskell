module Task.Internal

import Control.Monad.Ref

import Task.Universe
import Task.Event

%default total
%access export

infix  6 =~, /~


-- Types -----------------------------------------------------------------------

-- Tasks --

public export
data Task : (Type -> Type) -> Ty -> Type where
  -- Core
  Edit  : (val : Maybe (typeOf a)) -> Task m a
  Watch : MonadRef l m => l (typeOf a) -> Task m a
  -- Parallel
  All   : Show (typeOf a) => Show (typeOf b) => (left : Task m a) -> (right : Task m b) -> Task m (PAIR a b)
  -- Choice
  Any   : Show (typeOf a) => (left : Task m a) -> (right : Task m a) -> Task m a
  One   : Show (typeOf a) => (left : Task m a) -> (right : Task m a) -> Task m a
  Fail  : Task m a
  -- Sequence
  Then  : Show (typeOf a) => (this : Task m a) -> (next : typeOf a -> Task m b) -> Task m b
  Next  : Show (typeOf a) => (this : Task m a) -> (next : typeOf a -> Task m b) -> Task m b
  -- Labels
  Label : Show (typeOf a) => Label -> (this : Task m a) -> Task m a


-- Labels --

||| Get the current label, if one
label : Task m a -> Maybe Label
label (Label l _) = Just l
label _           = Nothing

||| Remove as much labels as possible from a task.
|||
||| Usefull to deeply match task constructors while ignoring labels.
delabel : Task m a -> Task m a
delabel (Label _ t) = delabel t
delabel t           = t

||| Match a label to a task.
(=~) : Label -> Task m a -> Bool
(=~) k (Label l _) = l == l
(=~) _ _           = False

||| Negation of `(=~)`.
(/~) : Label -> Task m a -> Bool
(/~) l t = not (l =~ t)

||| Collect all labels in an external choice
labels : Task m a -> List Label
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
find : Label -> Task m a -> Maybe Path
find k (Label l this) with ( k == l )
  | True                = Just GoHere
  | False               = find k this
find k (One left right) = map GoLeft (find k left) <|> map GoRight (find k right)
-- --FIXME: should we can send pick-events through to the lhs of a step (see also `labels`)?
-- find k (Then this _)    = find k this
-- find k (Next this _)    = find k this
find k _                = Nothing

||| Check if a task constructor keeps its label after stepping or loses it.
keeper : Task m a -> Bool
keeper (Edit _)  = True
keeper (All _ _) = True
keeper (Fail)    = True
keeper _         = False
