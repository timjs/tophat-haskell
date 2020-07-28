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
