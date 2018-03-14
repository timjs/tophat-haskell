// Implementation of the iTasks semantics from the paper
// Plasmeijer et al 2010 "iTasks for a Change..."

implementation module itasks

import StdEnv

import Properties
import Util

instance toString TaskId where
  toString i = foldl (\s n -> s +++ toString n) "" i

instance toString EditorValue where
  toString (i, (User u), v) = toString i +++ " " +++ u +++ toString v

instance toString Value where
  toString (v::Int) = toString v
  toString (v::String) = v
  toString (v::Bool) = toString v
  toString _ = abort "no toString instance for value"

instance toString Event where
  toString Refresh = "Refresh"
  toString (Commit taskId) = "Commit " +++ toString taskId
  toString (Update taskId value) = "Update " +++ toString taskId +++ " " +++ toString value

mkTaskId :: TaskId
mkTaskId = [0]

subIds :: TaskId -> [TaskId]
subIds i = [[n:i] \\ n <- [0..]]

mkState :: *World -> State
mkState world = { world = world, editors = [], errors = [] }

showGui :: State -> State
showGui s
  # (console,world) = stdio s.world
  # console = foldl (\console message = fwrites (message +++ "\n") console) console s.errors
  # console = fwrites "Editors:\n" console
  # console = foldl (\console editor = fwrites (toString editor +++ "\n") console) console s.editors
  # (_,world) = fclose console world
  = { s & world = world , errors = [] }

getNextEvent :: State -> (Event, State)
getNextEvent s
 # (console,world) = stdio s.world
 # (eventString,console) = freadline console
 # (_,world) = fclose console world
 = (parseEvent eventString, { s & world = world })

edit :: a -> STask a | TC a
edit value = \i p e s = case e of
  Commit tid
    | tid == i -> (NF value, showError ("committed " +++ toString tid) s)
  Update tid (newVal :: a^)
    | tid == i -> (Redex (edit newVal), addEditor i p newVal s)
  _            -> (Redex (edit value), addEditor i p value s)


addEditor :: TaskId Properties a State -> State | TC a
addEditor i ps value s = { s & editors = [(i, getProperty ps, dynamic value) : s.editors] }


normalize :: (STask a) *World -> (a, *World)
normalize task world = toNormalForm (startTask task Refresh (mkState world))

toNormalForm (NF value, s) = (value, s.world)
toNormalForm (Redex task, s) = toNormalForm (reduce task (getNextEvent (showGui s)))

reduce :: (STask a) (Event, State) -> (Reduct a, State)
reduce task (event, s)
  | valid event s.editors = startTask task event s
  | otherwise = (Redex task, showError ("Invalid event: " +++ (toString event)) s)

showError message s = { s & errors = [message : s.errors] }

valid :: Event [EditorValue] -> Bool
valid (Commit taskId) editors = isMember taskId [ i \\ (i,_,_) <- editors]
valid (Update taskId _) editors = isMember taskId [ i \\ (i,_,_) <- editors]
valid _ _ = True

startTask task event s = task mkTaskId mkProperties event { s & editors = [] }

retrn :: a -> STask a
retrn v = \_ _ _ s -> (NF v, s)

(-||-) infixr 3 :: (STask a) (STask a) -> STask a
(-||-) taskLeft taskRight = \taskId props event s =
  case taskLeft (subIds taskId !! 0) props event s of
    (NF leftVal, s) -> (NF leftVal, s)
    (Redex newLeft, s) ->
      case taskRight (subIds taskId !! 1) props event s of
        (NF rightVal, s) -> (NF rightVal, s)
        (Redex newRight, s) -> (Redex (newLeft -||- newRight), s)

(-&&-) infixr 4 :: (STask a) (STask b) -> STask (a, b)
(-&&-) taskLeft taskRigt = \taskId props event s =
  let (resultLeft, s1) = taskLeft (subIds taskId !! 0) props event s
      (resultRigt, s2) = taskRigt (subIds taskId !! 1) props event s1
  in
      case (resultLeft, resultRigt) of
        (NF valueLeft, NF valueRight) -> (NF (valueLeft, valueRight), s2)
        (NF l        , Redex r)       -> (Redex (retrn l -&&- r)    , s2)
        (Redex l     , NF r)          -> (Redex (l -&&- retrn r)    , s2)
        (Redex l     , Redex r)       -> (Redex (l -&&- r)          , s2)

(>>=) infixl 1 :: (STask a) (a -> STask b) -> STask b
(>>=) taskLeft aToB = \taskId props event s =
  case taskLeft (subIds taskId !! 0) props event s of
    (NF valueLeft, s1) -> setTaskId (\i -> subIds i !! 1) (aToB valueLeft) taskId props Refresh s1
    (Redex redexLeft, s1) -> (Redex (redexLeft >>= aToB), s1) 

setTaskId :: (TaskId -> TaskId) (STask a) -> STask a
setTaskId f task = \taskId props event s =
  case task (f taskId) props event s of
    (Redex newTask, s1) -> (Redex (setTaskId f newTask), s1)
    redex -> redex
