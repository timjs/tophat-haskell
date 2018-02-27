module Main where

import Data.Maybe

type ID = String

type UI = String

data Value a
  = NoValue
  | JustValue a
  deriving Show

data Task a = Task UI (Value a) (Event -> Task a)

data Event
  = EditorEvent ID String
  | StepEvent ID
  deriving Show

readValue :: Read a => String -> Value a
readValue input = maybe NoValue JustValue $ fmap fst $ listToMaybe $ reads input

editor :: (Show a, Read a) => ID -> (Value a) -> (Task a)
editor taskId value = Task ui value step
  where
    ui = "editor " ++ taskId ++ " " ++ show value ++ "\n"
    step (EditorEvent evId newVal)
      | evId == taskId = editor taskId (readValue newVal)
    step _ = editor taskId value


bind :: ID -> (Task a) -> (Value a -> Task b) -> (Task b)
bind taskId lhs@(Task uiLhs valueLhs stepLhs) rhs = Task ui NoValue step
  where
    ui = uiLhs ++ "step " ++ taskId ++ "\n"
    step e@(StepEvent evId)
      | evId == taskId = rhs (valueLhs)
    step e = bind taskId (stepLhs e) rhs


parAnd :: Task a -> Task b -> Task (a, b)
parAnd (Task uiLhs valueLhs stepLhs) (Task uiRhs valueRhs stepRhs) = Task ui value step
  where
    ui = uiLhs ++ uiRhs
    value = case (valueLhs, valueRhs) of
      (JustValue l, JustValue r) -> JustValue (l, r)
      _ -> NoValue
    step e =
      let
        newLhs = stepLhs e
        newRhs = stepRhs e
      in
        parAnd newLhs newRhs


runTask (Task ui value step) = do
  putStrLn $ ui
  ev <- getEvent
  runTask $ step ev


-- "ed 0 fooo" for an editor event with id 0 and value foo
-- "st 0" for a step event with id 0
getEvent :: IO Event
getEvent = do
  putStr "event? "
  input <- getLine
  case words input of
    ["ed",id,val] -> return (EditorEvent id val)
    ["ed",id]     -> return (EditorEvent id "")
    ["st",id]     -> return (StepEvent id)
    _             -> putStrLn "parse error" >> getEvent

-- Example tasks
singleEditor :: Task Int
singleEditor = editor "0" NoValue
oneStep :: Task String
oneStep = bind "1" (editor "0" (NoValue::Value Int)) (\x -> editor "2" $ case x of
  NoValue -> JustValue ("WTF???")
  JustValue v -> JustValue (show v ++ "w00t")
  )
onePara = parAnd (editor "0" (NoValue::Value Int)) (editor "1" (NoValue::Value String))
paraThenStep = bind "2" onePara (\x -> editor "3" x)
twoStepsInPara =
  parAnd
    (bind "0" (editor "1" (NoValue::Value Int)) (\x -> editor "2" x))
    (bind "3" (editor "4" (NoValue::Value Int)) (\x -> editor "5" x))

main = runTask twoStepsInPara
