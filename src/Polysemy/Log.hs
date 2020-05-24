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

-- Effect ----------------------------------------------------------------------

data Severity
  = Error
  | Warning
  | Info

instance Pretty Severity where
  pretty = \case
    Error -> "!!"
    Warning -> "**"
    Info -> "=="

data Log i m a where
  Log :: Severity -> i -> Log i m ()

makeSem ''Log

-- Interpretations -------------------------------------------------------------

logToIO :: (Member (Embed IO) r, Pretty i) => Sem (Log i ': r) a -> Sem r a
logToIO = interpret \case
  Log s i -> embed <| print <| sep [pretty s, pretty i]
