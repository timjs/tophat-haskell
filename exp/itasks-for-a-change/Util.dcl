definition module Util

import itasks

// parseEvent "r" = Refresh
// parseEvent "c 01" = Commit [0, 1]
// parseEvent "u 101 42" = Update [101] (dynamic 42)
parseEvent :: String -> Event
