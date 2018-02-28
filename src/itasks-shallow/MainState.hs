module Main where

import Data.Maybe

type ID = String

type UI = String

type State = Int

data Value a
  = NoValue
  | JustValue a
  deriving Show

-- Every task has a UI, a current value, and a continuation function
-- This data type should be abstract. The only way to create tasks for a
-- user should be the functions editor, bind, and parAnd.
data Task a = Task (State -> (UI, (Value a), (Event -> Task a), State))

data Event
  = EditorEvent ID String
  | StepEvent ID
  deriving Show

readValue :: Read a => String -> Value a
readValue input = maybe NoValue JustValue $ fmap fst $ listToMaybe $ reads input

-- Want:
--   getState >>= \x -> editor "0" x

getState :: Task State
getState = Task mkGetState
  where
    mkGetState s = (ui, value, step, state)
      where
        ui = "getState " ++ show s
        value = (JustValue s)
        step = const getState
        state = s


putState :: State -> Task ()
putState newState = Task mkPutState
  where
    mkPutState _ = (ui, value, step, state)
      where
        ui = "putState " ++ show newState
        value = (JustValue ())
        step = const (putState newState)
        state = newState


editor :: (Show a, Read a) => ID -> (Value a) -> (Task a)
editor taskId value = Task mkEditor
  where
    mkEditor s = (ui, value, step, state)
      where
        ui = "editor " ++ taskId ++ " " ++ show value ++ "\n"
        step (EditorEvent evId newVal)
          | evId == taskId = editor taskId (readValue newVal)
        step _ = editor taskId value
        state = s


bind :: ID -> (Task a) -> (a -> Task b) -> (Task b)
bind taskId lhs@(Task f) rhs = Task mkBind
  where
    mkBind s = (ui, value, step, state)
      where
        (uiLhs, valueLhs, stepLhs, stateLhs) = f s
        ui = uiLhs ++ case valueLhs of
          (JustValue _) -> "step " ++ taskId ++ "\n"
          NoValue -> "(disabled) step " ++ taskId ++ "\n"
        value = NoValue
        step e@(StepEvent evId) | evId == taskId = case valueLhs of
          (JustValue v) -> rhs v
          NoValue -> bind taskId lhs rhs
        step e = bind taskId (stepLhs e) rhs
        state = stateLhs


parAnd :: Task a -> Task b -> Task (a, b)
parAnd (Task fLhs) (Task fRhs) = Task mkParAnd
  where
    mkParAnd s = (ui, value, step, state)
      where
        (uiLhs, valueLhs, stepLhs, stateLhs) = fLhs s
        (uiRhs, valueRhs, stepRhs, stateRhs) = fRhs stateLhs
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
        state = stateRhs


runTask s (Task f) = do
  let (ui, value, step, s2) = f s
  putStrLn $ "current state: " ++ show s
  putStrLn $ ui
  ev <- getEvent
  runTask s2 $ step ev


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

{-

-- Example tasks
singleEditor :: Task Int
singleEditor = editor "0" NoValue

oneStep :: Task String
oneStep = bind "1" (editor "0" (NoValue::Value Int)) (\x -> editor "2" (JustValue $ show x ++ "w00t"))

onePara = parAnd (editor "0" (NoValue::Value Int)) (editor "1" (NoValue::Value String))

paraThenStep = bind "2" onePara (\x -> editor "3" (JustValue x))

twoStepsInPara =
  parAnd
    (bind "0" (editor "1" (NoValue::Value Int)) (\x -> editor "2" (JustValue x)))
    (bind "3" (editor "4" (NoValue::Value Int)) (\x -> editor "5" (JustValue x)))

-}

statefulThing :: Task Int
statefulThing = bind "1" (getState) (\x -> editor "2" (JustValue x))

editState :: Task ()
editState = bind "1" (getState) (\x -> bind "3" (editor "2" (JustValue x)) (\y -> putState y))

writeState :: Task ()
writeState = bind "1" (editor "2" NoValue) (\y -> putState y)

main = runTask 42 writeState
