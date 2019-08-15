module Guis where

import Data.Task
import Data.Task.Run


-- Temperature conversion ------------------------------------------------------

c2f :: Double -> Double
c2f c' = (c' * 9.0 / 5.0) + 32.0

f2c :: Double -> Double
f2c f' = ((f' - 32.0) * 5.0) / 9.0

-- This will loop indefinitely and is not part of the TopHat language (recursion!)
temperature :: ( Double, Double ) -> Task m a
temperature ( c, f ) = do
  n <- map Left (update c) <|> map Right (update f)
  case n of
    Left c' -> temperature ( c', c2f c' )
    Right f' -> temperature ( f2c f', f' )

-- This will also loop indefinitely...
temperature' :: ( Double, Double ) -> Task m Void
temperature' ( c, f ) = forever do
  n <- map Left (update c) <|> map Right (update f)
  case n of
    Left c' -> pure ( c', c2f c' )
    Right f' -> pure ( f2c f', f' )

-- This is OK, but we need a way to transform our view on the share (lenses!)
temperature'' :: Collaborative l m => Double -> Task m Double
temperature'' c = do
  r <- share c
  change r <|> change r -- (iso c2f f2c << r)
