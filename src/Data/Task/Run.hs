module Data.Task.Run where


import Preload


import Data.Task as Task
import Data.Task.Input as Input



-- Running ---------------------------------------------------------------------


getInput :: IO (Input Action)
getInput = do
  putText ">> "
  input <- getLine
  case input of
    "quit" -> exitSuccess
    _ ->
      case Input.parse (words input) of
        Right i -> do
          pure i
        Left msg -> do
          putStrLn msg
          getInput


{-
loop :: TaskIO a -> IO ()
loop task = do
  ui <- Task.ui task
  putStrLn ui
  is <- Task.inputs task
  putStrLn $ "Possibilities: " <> show is
  input <- getInput
  task_new <- _ task input
  loop task_new


run :: Show (typeOf a) -> Task a -> IO ()
run task = loop !(Task.initialise task)


main :: IO ()
main = run empties

-}
