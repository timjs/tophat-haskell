module Data.Task.Interact where

import Data.Task (Task)
import Data.Task.Input (Concrete, Input)
import qualified Data.Task.Input as Input
import qualified Data.Task.Observe as Task
import qualified Data.Task.Run as Task
import Data.Task.Run (NotApplicable, Steps)
import Polysemy
import Polysemy.Abort
import Polysemy.Interact
import Polysemy.Log
import Polysemy.Mutate
import Polysemy.Supply

---- Looping -------------------------------------------------------------------

loop ::
  Members '[Interact, Abort, Log Steps, Log NotApplicable, Supply Nat, Alloc h, Read h, Write h] r =>
  Task h r a ->
  Sem r b
loop t = do
  t' <- Task.initialise t
  go t'
  where
    go ::
      Members '[Interact, Abort, Log Steps, Log NotApplicable, Supply Nat, Alloc h, Read h, Write h] r =>
      Task h r a ->
      Sem r b
    go t' = do
      printLn neutral
      ui <- Task.ui t'
      printLn ui
      inputs <- Task.inputs t'
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
--   Task 'Global '[Interact, Abort, Log Steps, Log NotApplicable, Supply Nat, Alloc 'Global, Read 'Global, Write 'Global, Embed IO] a ->
--   IO b
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
