module Data.Universe

%default total

public export
interface DecEq ty => Universe ty where
  typeOf : ty -> Type
  defaultOf : (a : ty) -> typeOf a

-- record Univ where
--   constructor BigBang
--   Ty        : Type
--   typeOf    : Ty -> Type
--   defaultOf : (ty : Ty) -> typeOf ty
--   decEq     : (x : Ty) -> (y : Ty) -> Dec (x = y)
