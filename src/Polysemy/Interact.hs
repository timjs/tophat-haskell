{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Interact
  ( -- * Effect
    Interact (..),

    -- * Actions
    scanLn,
    printLn,
    print,

    -- * Interpretations
    interactToIO,
    interactToInputOutput,
    -- runInteract,
  )
where

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Interact m a where
  ScanLn :: Interact m Text
  PrintLn :: Text -> Interact m ()
  Print :: Text -> Interact m ()

makeSem ''Interact

interactToIO :: (Member (Embed IO) r) => Sem (Interact ': r) a -> Sem r a
interactToIO = interpret \case
  ScanLn -> embed getTextLn
  PrintLn msg -> embed <| putTextLn msg
  Print msg -> embed <| putText msg

interactToInputOutput :: Sem (Interact ': r) a -> Sem ((Input Text) ': (Output Text) ': r) a
interactToInputOutput = reinterpret2 \case
  ScanLn -> input
  PrintLn msg -> output msg
  Print msg -> output msg

-- runInteract :: List Text -> Sem (Interact ': r) a -> Sem r (List Text, a)
-- runInteract is =
--   interactToInputOutput >> runInputStream is >> runOutputMonoid pure

-- runInputStream ::
--   Stream i ->
--   Sem (Input i ': r) a ->
--   Sem r (Maybe a)
-- runInputStream is = _ << runState is << reinterpret \case
--   Input -> do
--     s <- gets uncons
--     case s of
--       Just (x, xs) -> do
--         put xs
--         pure (Just x)
--       Nothing -> pure Nothing
-- {-# INLINE runInputStream #-}

-- runOutputMonoid pure -- For each PrintLn in our program, consume an output by appending it to the list in a ([Text], a)
--   << runInputList is -- Treat each element of our list of Texts as a line of input
--   << reinterpret2 \case
--     -- Reinterpret our effect in terms of Input and Output
--     ScanLn -> getInput
--     PrintLn msg -> output msg
--     Print msg -> output msg
