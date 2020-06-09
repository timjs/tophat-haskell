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
    -- runInteractPure,
  )
where

import Polysemy

-- import Polysemy.Input
-- import Polysemy.Output

data Interact m a where
  ScanLn :: (Scan a, Reflect a) => Interact m a
  PrintLn :: Text -> Interact m ()
  Print :: Text -> Interact m ()

makeSem ''Interact

interactToIO :: (Member (Embed IO) r) => Sem (Interact ': r) a -> Sem r a
interactToIO = interpret \case
  ScanLn -> embed scanTextLn
  PrintLn msg -> embed <| putTextLn msg
  Print msg -> embed <| putText msg
  where
    scanTextLn :: forall a. (Scan a, Reflect a) => IO a
    scanTextLn = do
      text <- getTextLn
      case scan text of
        Just x -> pure x
        Nothing -> do
          putTextLn <| "*** Scan error, please enter a(n) " ++ debug tau
          scanTextLn
      where
        tau = typeRep :: TypeRep a

--FIXME: infinte!
-- runInteractPure :: List Text -> Sem (Interact ': r) a -> Sem r (List Text, a)
-- runInteractPure is =
--   runOutputMonoid pure -- For each PrintLn in our program, consume an output by appending it to the list in a ([Text], a)
--     << runInputList is -- Treat each element of our list of Texts as a line of input
--     << reinterpret2 \case
--       -- Reinterpret our effect in terms of Input and Output
--       ScanLn -> getInput
--       PrintLn msg -> output msg
--       Print msg -> output msg
--   where
--     getInput :: (Scan a) => Sem (Input (Maybe Text) ': r) a
--     getInput = do
--       next <- input
--       case next of
--         Just text -> case scan text of
--           Just x -> pure x
--           Nothing -> getInput
--         Nothing -> getInput
