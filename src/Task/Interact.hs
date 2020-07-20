module Task.Interact where

import Data.Store (RealWorld)
import Polysemy
import Polysemy.Abort
import Polysemy.Interact
import Polysemy.Log
import Polysemy.Mutate
import Polysemy.Supply
import Task (Task)
import Task.Input (Concrete, Input)
import qualified Task.Input as Input
import qualified Task.Observe as Task
import Task.Run (NotApplicable, Steps)
import qualified Task.Run as Task

---- Looping -------------------------------------------------------------------

loop ::
  Members '[Interact, Abort, Log Steps, Log NotApplicable, Supply Nat, Alloc h, Read h, Write h] r =>
  Task h a ->
  Sem r b
loop t = do
  t' <- Task.initialise t
  go t'
  where
    go ::
      Members '[Interact, Abort, Log Steps, Log NotApplicable, Supply Nat, Alloc h, Read h, Write h] r =>
      Task h a ->
      Sem r b
    go t' = do
      printLn neutral
      ui <- Task.ui t'
      printLn ui
      inputs <- Task.inputs t'
      if null inputs
        then do
          log Info Task.DidFinish
          abort
        else do
          printLn <| "Possibilities: " ++ display inputs
          input <- getUserInput
          t'' <- Task.interact input t'
          go t''
    getUserInput ::
      Members '[Interact, Abort] r =>
      Sem r (Input Concrete)
    getUserInput = do
      print ">> "
      line <- scanLn
      case line of
        "quit" -> abort
        "q" -> abort
        _ -> case Input.parse line of
          Right input -> pure input
          Left message -> do
            printLn message
            getUserInput

-- taskToIO ::
-- taskToIO :: Task 'Global a1 -> IO a2
taskToIO :: Task RealWorld a1 -> IO a2
taskToIO =
  loop
    >> interactToIO
    >> abortToIO
    >> logToIO @Steps
    >> logToIO @NotApplicable
    >> supplyToIO
    >> allocToIO
    >> readToIO
    >> writeToIO
    >> runM
