module Bigger where

import qualified Data.Text as Text
import Task
import Task.Interact
import Prelude hiding (guard, repeat)

---- Text editor ---------------------------------------------------------------

textEditor :: Reflect h => Task h ()
textEditor = do
  s_t <- share "Edit me :-)"
  s_r <- share (0, 0)
  repeat <| edit s_t s_r
  where
    edit :: Store h Text -> Store h (Int, Int) -> Task h ()
    edit s_t s_r = do
      (t, r) <- change s_t >< change s_r
      guard
        [ "Bold" ~> hasSelection r t ~> s_t <<= makeup "*" r,
          "Italic" ~> hasSelection r t ~> s_t <<= makeup "/" r
        ]

    hasSelection :: (Int, Int) -> Text -> Bool
    hasSelection (x, y) t = 0 <= x && x < y && y < Text.length t

    makeup :: Text -> (Int, Int) -> Text -> Text
    makeup m (x, y) t
      | (a, rest) <- Text.splitAt x t,
        (b, c) <- Text.splitAt (y - x) rest =
        a ++ m ++ b ++ m ++ c

---- Chat session --------------------------------------------------------------

chatSession :: Reflect h => Task h (((Text, ()), (Text, ())), (Text, ()))
chatSession = do
  history <- share ""
  chat "Tim" history >< chat "Nico" history >< chat "Markus" history
  where
    chat :: Text -> Store h Text -> Task h (Text, ())
    chat name history =
      watch history >< repeat (enter >>* ["Send" ~> append history name])

    append :: Store h Text -> Text -> Text -> Task h ()
    append history name msg =
      history <<= \h -> h ++ "\n" ++ name ++ ": '" ++ msg ++ "'"
