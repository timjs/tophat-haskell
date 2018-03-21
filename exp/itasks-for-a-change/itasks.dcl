// Implementation of the iTasks semantics from the paper
// Plasmeijer et al 2010 "iTasks for a Change..."

definition module itasks

import StdDynamic
import StdString
from GenEq import generic gEq

import Properties

:: STask a :== TaskId Properties Event State -> *(Reduct a, State)
:: TaskId :== [Int]
:: *State = {editors :: [EditorValue], world :: *World, errors :: [String]}
:: EditorValue :== (TaskId, User, Value)
:: Reduct a = NF a | Redex (STask a) // NF = normal form
:: Event = Commit TaskId
         | Update TaskId Value
         | Refresh
:: Value :== Dynamic

mkTaskId :: TaskId
subIds :: TaskId -> [TaskId]

mkState :: *World -> State

showGui :: State -> State
getNextEvent :: State -> (Event, State)

normalize :: (STask a) *World -> (a, *World)


edit :: a -> STask a | TC a
retrn :: a -> STask a
(-||-) infixr 3 :: (STask a) (STask a) -> STask a
(-&&-) infixr 4 :: (STask a) (STask b) -> STask (a, b)
(>>=) infixl 1 :: (STask a) (a -> STask b) -> STask b

instance toString EditorValue, Value, Event, TaskId
