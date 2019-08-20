module Guis where

import Data.Task
import Lens.Simple (iso)


-- Counter ---------------------------------------------------------------------

-- This is not part of the TopHat language (recursion!)
counter :: Int -> Task m Void
counter start = do
  count <- view start
  pick
    [ ( "Increment", counter <| succ count )
    , ( "Decrement", counter <| pred count )
    ]

-- `forever` is not a proper (monadic) fixpoint combinator,
-- so we cannot feed new pure values into our task.
-- Otherwise we could do:
--   counter' :: Int -> Task m Int
--   counter' start = loop start \count -> do
--     _ <- view count
--     pick
--       [ ( "Increment", pure <| succ count )
--       , ( "Decrement", pure <| pred count )
--       ]

-- Therefore we need a share!
counter'' :: forall r m. Collaborative r m => Int -> Task m Void
counter'' start = do
  c <- share start
  forever do
    _ <- watch c
    pick
      [ ( "Increment", c <<= succ )
      , ( "Decrement", c <<= pred )
      ]


-- Temperature conversion ------------------------------------------------------

c2f :: Double -> Double
c2f c' = (c' * 9.0 / 5.0) + 32.0

f2c :: Double -> Double
f2c f' = ((f' - 32.0) * 5.0) / 9.0

-- This is not part of the TopHat language (it uses recursion!)
-- and will loop indefinitely...
-- Also, both `c` and `f` have a value, so when updating `f`,
-- we would always recieve the old `c` value tagged with `Left`...
temperature :: ( Double, Double ) -> Task m a
temperature ( c, f ) = do
  n <- map Left (update c) <|> map Right (update f)
  case n of
    Left c' -> temperature ( c', c2f c' )
    Right f' -> temperature ( f2c f', f' )

-- Because steps do not wait for an event (they fire automatically if there is a value),
-- this will also loop indefinitely...
temperature' :: ( Double, Double ) -> Task m Void
temperature' ( c, f ) = forever do
  n <- map Left (update c) <|> map Right (update f)
  case n of
    Left c' -> pure ( c', c2f c' )
    Right f' -> pure ( f2c f', f' )

-- with shares, we do not need any recursion.
-- Recursion on editing is built in.
-- However, we need a way to transform our view on shares: lenses!
temperature'' :: Collaborative r m => Double -> Task m Double
temperature'' c = do
  r <- share c
  change r <& change (focus (iso c2f f2c) r)


-- Flight booker ---------------------------------------------------------------

-- We use type synonyms instead of new data types so we do not have to extend
-- the value parser from terminal input.
type Date = Int
type Flight = Either Date ( Date, Date )

book :: Task m Flight
book = do
  flight <- enter
  pick
    [ ( "Continue", case flight of
        Left _ -> pure flight
        Right ( d1, d2 ) -> if d1 < d2 then pure flight else empty
      )
    ]
