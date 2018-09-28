module Data.Universe


%default total



public export
interface DecEq t => Universe t where
  typeOf : t -> Type

  show : (b : t) -> Show (typeOf b)
  eq : (b : t) -> Eq (typeOf b)
