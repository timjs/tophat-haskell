{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Log
  ( -- * Effect
    Log (..),
    Severity (..),

    -- * Actions
    log,

    -- * Interpretations
    logToOutput,
    logToIO,
  )
where

import Polysemy
import Polysemy.Output

---- Effect --------------------------------------------------------------------

data Severity
  = Error
  | Warning
  | Info

instance Display Severity where
  display = \case
    Error -> "!!"
    Warning -> "**"
    Info -> "=="

data Log i m a where
  Log :: Severity -> i -> Log i m ()

makeSem ''Log

---- Interpretations -----------------------------------------------------------

logToOutput :: (Display i) => Sem (Log i ': r) a -> Sem (Output Text ': r) a
logToOutput = reinterpret \case
  Log s i -> output <| unwords [display s, display i]

logToIO :: forall i a r. (Member (Embed IO) r, Display i) => Sem (Log i ': r) a -> Sem r a
logToIO = logToOutput >> runOutputSem putTextLn
