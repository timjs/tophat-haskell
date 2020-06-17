module Data.Some
  ( Some (..),
    pack,
    unpack,
  )
where

data Some f where
  Some :: (Reflect (f a), Eq (f a)) => f a -> Some f

pack :: forall f a. (Reflect (f a), Eq (f a)) => f a -> Some f
pack = Some

unpack :: forall f a. (Reflect (f a)) => Some f -> Maybe (f a)
unpack (Some x)
  | Just Refl <- x ~: r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (f a)

instance Eq (Some h) where
  Some x == Some y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
