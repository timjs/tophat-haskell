module Data.Basic
  ( Basic,
  )
where

import Data.Aeson (FromJSON, ToJSON)

---- Basic types ---------------------------------------------------------------

-- | Types `a` which can be edited by an end user.
-- |
-- | They should be:
-- | * `Display` to show them in a user interface.
-- | * `Scan` to parse them from input.
-- | * `Reflect` to match input type and editor type witch each other.
-- |
-- | It is defined as a class synonym instead of a type synonym to let GHC
-- | give better error messges.
-- |
-- | FIXME: Maybe also 'Arbitrary' and `Coarbitrary` ?
class (Display a, Scan a, Eq a, Reflect a, ToJSON a, FromJSON a) => Basic a

instance (Display a, Scan a, Eq a, Reflect a, ToJSON a, FromJSON a) => Basic a
