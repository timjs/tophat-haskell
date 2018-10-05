module Data.Surely where


import Preload


-- | A `Surely a` represents an input value.
-- | It can be exactly the value `a`,
-- | or anything which matches the type of `a`.
-- |
-- | It is similar to `Maybe a`,
-- | except that `Anything` is equal to every exact value.
-- | That means that `Semantics.inputs` can produce `Change` inputs
-- | without specifying a specific input value.
data Surely a
  = Exactly a
  | Anything
  deriving (Show, Eq, Ord, Functor)


toMaybe :: Surely a -> Maybe a
toMaybe (Exactly x) = Just x
toMaybe (Anything)  = Nothing


toSurely :: Maybe a -> Surely a
toSurely (Just x)  = Exactly x
toSurely (Nothing) = Anything
