module Data.Universe

%default total

public export
interface DecEq ty => Universe ty where
  typeOf : ty -> Type
  defaultOf : (a : ty) -> typeOf a
