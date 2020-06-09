{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Log
  ( -- * Effect
    Log (..),
    Severity (..),

    -- * Actions
    log,

    -- * Interpretations
    logToIO,
  )
where

import Polysemy

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

logToIO :: (Member (Embed IO) r, Debug i) => Sem (Log i ': r) a -> Sem r a
logToIO = interpret \case
  Log s i -> embed <| putTextLn <| unwords [display s, debug i]
