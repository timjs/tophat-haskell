module Main where

-- Draft for a deep embedding of simple tasks. Tasks are syntax trees. There
-- are two semantics, the evaluation semantics and the user interface
-- semantics. No stable/unstable values yet. Therefore ParOr is not possible.

-- If we follow the principle of deep embedding strictly, then all semantics
-- should be separated. There are three different semantics: The step
-- semantics, the value semantics and the ui semantics. They come as separate
-- functions which _interpret_ tasks.
--
--   TaskTree -> Value
--   TaskTree -> UI
--   TaskTree -> Event -> TaskTree
--
-- In a shallow embedding, all semantics are combined into one data type, which
-- _is_ the task. The type must be recursive.
--
--   data Task = Task (Value, UI, Event -> Task)
--
-- Note how the arrow of the step semantics is inside the tuple.
--
-- Bas says: the real iTasks implementation is neither deep nor shallow, it
-- uses a different technique. Tasks never change, they never rewrite. TaskTree
-- is not the same as the AST of a deep embedding.
-- iTasks is shallow-ish in the sense that the arrow _is_ the task.
--
--   :: Task = Task (Event TaskTree -> (Value, TaskTree))
--
-- All tasks are interpreters of their TaskTree. The structure of a whole
-- workflow is fixed. It re-routes events as steps are being taken.

type ID = String

type UI = String

data Value
  = NoValue -- required for step combinator
  | JustValue String
  deriving Show

-- Assumption: all IDs in one TaskTree are unique
data TaskTree
  = Editor ID Value
  | Bind ID TaskTree (Value -> TaskTree) -- This arrow can be any user-defined Haskell code
  | ParAnd TaskTree TaskTree

data Event
  = EditorEvent ID Value
  | StepEvent ID
  deriving Show


-- Evaluation semantics
value :: TaskTree -> Value
value (Editor _ v) = v
value (Bind _ _ _) = NoValue
value (ParAnd lhs rhs) = JustValue $ "(" ++ show (value lhs) ++ "," ++ show (value rhs) ++ ")"


-- Step semantics
step :: Event -> TaskTree -> TaskTree -- This arrow is completely defined by the semantics

-- Editors change their value with an appropriate event
step (EditorEvent evId newVal) (Editor taskId _)
  | evId == taskId = Editor taskId newVal
step _ t@(Editor _ _) = t

-- Bind takes a step when its button is pressed
step e@(StepEvent evId) (Bind taskId lhs rhs)
  | evId == taskId = rhs (value lhs) -- The step semantics uses the value semantics. Weird.
step e (Bind taskId lhs rhs) =
    Bind taskId (step e lhs) rhs

-- Parallel distributes the event to the subtasks
step e (ParAnd lhs rhs) =
  let
    newLhs = step e lhs
    newRhs = step e rhs
  in
    ParAnd newLhs newRhs


-- User interface semantics
ui :: TaskTree -> UI
ui (Editor id val) = "editor " ++ id ++ " " ++ show val ++ "\n"
ui (Bind id lhs _) = ui lhs ++ "step " ++ id ++ "\n"
ui (ParAnd lhs rhs) = ui lhs ++ ui rhs


runTask task = do
  putStrLn $ ui task
  ev <- getEvent
  runTask $ step ev task


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
