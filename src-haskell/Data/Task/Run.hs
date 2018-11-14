module Data.Task.Run where


import Preload


import Data.Task as Task
import Data.Task.Input as Input



{- Running ---------------------------------------------------------------------


getInput :: IO Input
getInput = do
  putStr ">> "
  input <- getLine
  case input of
    "quit" -> exit 0
    _ ->
      case Input.parse (words input) of
        Right event -> do
          edit event
        Left msg -> do
          putStrLn msg
          getInput


loop :: Show (typeOf a) -> Task a -> IO ()
loop task = do
  putStrLn !(Task.ui task)
  putStrLn $ "Possibilities: " <> show !(Task.inputs task)
  event <- getInput
  loop !(_ task event)


run :: Show (typeOf a) -> Task a -> IO ()
run task = loop !(Task.initialise task)


main :: IO ()
main = run empties

-}
