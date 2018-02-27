module Main where

-- Draft for a deep embedding of simple tasks. Tasks are syntax trees. There
-- are two semantics, the evaluation semantics and the user interface
-- semantics. No stable/unstable values yet. Therefore ParOr is not possible.

type ID = String

data Value
  = NoValue -- required for step combinator
  | JustValue String
  deriving Show

-- Assumption: all IDs in one TaskTree are unique
data TaskTree
  = Editor ID Value
  | Bind ID TaskTree (Value -> TaskTree)
  | ParAnd TaskTree TaskTree

data Event
  = EditorEvent ID Value
  | StepEvent ID
  deriving Show

-- Evaluation semantics
sem :: Event -> TaskTree -> (Value, TaskTree)

-- Editors change their value with an appropriate event
sem (EditorEvent evId newVal) (Editor taskId oldVal)
  | evId == taskId = (newVal, Editor taskId newVal)
sem _ t@(Editor _ oldVal) = (oldVal, t)

-- Bind takes a step when its button is pressed
sem e@(StepEvent evId) (Bind taskId lhs rhs)
  | evId == taskId =
    -- This is a bit ugly, because to get the value of the left hand side, we
    -- must send it some event. By assumption all IDs are unique, so lhs should
    -- not react to the event. In iTasks they use refresh events if I remember
    -- correctly.
    let (leftVal, _) = sem e lhs in
    (NoValue, rhs leftVal)
sem e (Bind taskId lhs rhs) =
    let (_, newLhs) = sem e lhs in
    (NoValue, Bind taskId newLhs rhs)

-- Parallel distributes the event to the subtasks
sem e (ParAnd lhs rhs) =
  let
    (lVal, newLhs) = sem e lhs
    (rVal, newRhs) = sem e rhs
  in
    (JustValue $ "(" ++ show lVal ++ "," ++ show rVal ++ ")", ParAnd newLhs newRhs)

-- User interface semantics
ui :: TaskTree -> String
ui (Editor id val) = "editor " ++ id ++ " " ++ show val ++ "\n"
ui (Bind id lhs _) = ui lhs ++ "step " ++ id ++ "\n"
ui (ParAnd lhs rhs) = ui lhs ++ ui rhs

runTask task = do
  putStrLn $ ui task
  ev <- getEvent
  runTask $ snd $ sem ev task

-- "ed 0 fooo" for an editor event with id 0 and value foo
-- "st 0" for a step event with id 0
getEvent :: IO Event
getEvent = do
  putStr "event? "
  input <- getLine
  case words input of
    ["ed",id,val] -> return (EditorEvent id (JustValue val))
    ["ed",id]     -> return (EditorEvent id NoValue)
    ["st",id]     -> return (StepEvent id)
    _             -> putStrLn "parse error" >> getEvent

-- Example tasks
oneStep = Bind "1" (Editor "0" NoValue) (\x -> Editor "2" $ case x of
  NoValue -> JustValue ("WTF???")
  JustValue v -> JustValue (v ++ "w00t")
  )
onePara = ParAnd (Editor "0" NoValue) (Editor "1" NoValue)
paraThenStep = Bind "2" onePara (\x -> Editor "3" x)
twoStepsInPara =
  ParAnd
    (Bind "0" (Editor "1" NoValue) (\x -> Editor "2" x))
    (Bind "3" (Editor "4" NoValue) (\x -> Editor "5" x))

main = runTask oneStep
