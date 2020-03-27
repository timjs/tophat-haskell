module Data.Editable
  ( Editable,
  )
where

-- | Types `a` which can be edited by an end user.
-- |
-- | They should be:
-- | * `Pretty` to show them in a user interface.
-- | * `Read` to parse them from input.
-- | * `Typeable` to match input type and editor type witch each other.
-- |
-- | It is defined as a class synonym instead of a type synonym to let GHC
-- | give better error messges.
-- |
-- | FIXME: Maybe also 'Arbitrary' and `Coarbitrary` ?
class (Pretty a, Read a, Eq a, Typeable a) => Editable a

instance (Pretty a, Read a, Eq a, Typeable a) => Editable a
