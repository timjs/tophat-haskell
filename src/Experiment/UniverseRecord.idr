module Data.Universe

%default total

public export
record Universe where
  constructor BigBang
  Ty        : Type
  typeOf    : Ty -> Type
  defaultOf : (t : Ty) -> typeOf t
  decEq     : (t : Ty) -> (s : Ty) -> Dec (t = s)
