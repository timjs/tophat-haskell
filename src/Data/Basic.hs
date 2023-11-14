module Data.Basic
  ( Basic,
  )
where

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
class (Debug a, Display a, Scan a, Eq a, Typeable a, ToJSON a, FromJSON a) => Basic a

instance (Debug a, Display a, Scan a, Eq a, Typeable a, ToJSON a, FromJSON a) => Basic a
