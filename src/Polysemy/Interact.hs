{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Interact
  ( -- * Effect
    Interact (..),

    -- * Actions
    getLine,
    putLine,

    -- * Interpretations
    interactToIO,
    runInteractPure,
  )
where

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Interact m a where
  GetLine :: Interact m Text
  PutLine :: Text -> Interact m ()

makeSem ''Interact

interactToIO :: Member (Embed IO) r => Sem (Interact ': r) a -> Sem r a
interactToIO = interpret \case
  GetLine -> embed getTextLine
  PutLine msg -> embed <| putTextLn msg

runInteractPure :: List Text -> Sem (Interact ': r) a -> Sem r (List Text, a)
runInteractPure is =
  runOutputMonoid pure -- For each PutLine in our program, consume an output by appending it to the list in a ([Text], a)
    << runInputList is -- Treat each element of our list of Texts as a line of input
    << reinterpret2 \case
      -- Reinterpret our effect in terms of Input and Output
      GetLine -> maybe "" identity <|| input
      PutLine msg -> output msg
