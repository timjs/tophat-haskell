module Task.Universe

import Data.String
import Helpers

%default total
%access public export


-- Basic universe --------------------------------------------------------------

namespace Basic

  data Ty
    = UnitTy
    | BoolTy
    | IntTy
    | StringTy

  typeOf : Ty -> Type
  typeOf UnitTy   = ()
  typeOf BoolTy   = Bool
  typeOf IntTy    = Int
  typeOf StringTy = String

  defaultOf : (ty : Ty) -> typeOf ty
  defaultOf UnitTy   = ()
  defaultOf BoolTy   = False
  defaultOf IntTy    = 0
  defaultOf StringTy = ""


  -- Lemmas --

  Uninhabited (UnitTy = BoolTy) where
    uninhabited Refl impossible

  Uninhabited (UnitTy = IntTy) where
    uninhabited Refl impossible

  Uninhabited (UnitTy = StringTy) where
    uninhabited Refl impossible

  Uninhabited (BoolTy = IntTy) where
    uninhabited Refl impossible

  Uninhabited (BoolTy = StringTy) where
    uninhabited Refl impossible

  Uninhabited (IntTy = StringTy) where
    uninhabited Refl impossible


  -- Decidablility --

  DecEq Ty where
    decEq UnitTy   UnitTy   = Yes Refl
    decEq BoolTy   BoolTy   = Yes Refl
    decEq IntTy    IntTy    = Yes Refl
    decEq StringTy StringTy = Yes Refl
    decEq UnitTy   BoolTy   = No absurd
    decEq BoolTy   UnitTy   = No (negEqSym absurd)
    decEq UnitTy   IntTy    = No absurd
    decEq IntTy    BoolTy   = No (negEqSym absurd)
    decEq UnitTy   StringTy = No absurd
    decEq StringTy UnitTy   = No (negEqSym absurd)
    decEq BoolTy   IntTy    = No absurd
    decEq IntTy    BoolTy   = No (negEqSym absurd)
    decEq BoolTy   StringTy = No absurd
    decEq StringTy BoolTy   = No (negEqSym absurd)
    decEq IntTy    StringTy = No absurd
    decEq StringTy IntTy    = No (negEqSym absurd)


  -- Parsing --

  parse : String -> Maybe (b : Ty ** typeOf b)
  parse "()"                                                        = Just $ (UnitTy ** ())
  parse "True"                                                      = Just $ (BoolTy ** True)
  parse "False"                                                     = Just $ (BoolTy ** False)
  parse s   with (the (Maybe Int) (parseInteger s))
    parse s | (Just int)                                            = Just $ (IntTy ** int)
    parse s | Nothing                        with (decons s)
      parse (between '"' '"' rest) | Nothing | (Multi '"' '"' rest) = Just $ (StringTy ** rest)
      parse _                      | Nothing | _                    = Nothing


-- Full universe ---------------------------------------------------------------

data Ty
  = PairTy Universe.Ty Universe.Ty
  | ListTy Universe.Ty
  | BasicTy Basic.Ty

||| Conversion of Task types to Idris types.
typeOf : Universe.Ty -> Type
typeOf (PairTy x y) = ( typeOf x, typeOf y )
typeOf (ListTy x)   = List (typeOf x)
typeOf (BasicTy b)  = Basic.typeOf b

defaultOf : (ty : Universe.Ty) -> Universe.typeOf ty
defaultOf (PairTy x y) = ( defaultOf x, defaultOf y )
defaultOf (ListTy x)   = []
defaultOf (BasicTy b)  = Basic.defaultOf b


-- Lemmas --

Uninhabited (PairTy _ _ = ListTy _) where
  uninhabited Refl impossible

Uninhabited (PairTy _ _ = BasicTy _) where
  uninhabited Refl impossible

Uninhabited (ListTy _ = BasicTy _) where
  uninhabited Refl impossible

list_neq : (a = b -> Void) -> (ListTy a = ListTy b) -> Void
list_neq contra Refl = contra Refl

basic_neq : (a = b -> Void) -> (BasicTy a = BasicTy b) -> Void
basic_neq contra Refl = contra Refl

snd_neq : (y = y' -> Void) -> (PairTy x y = PairTy x y') -> Void
snd_neq contra Refl = contra Refl

fst_neq : (x = x' -> Void) -> (PairTy x y = PairTy x' y) -> Void
fst_neq contra Refl = contra Refl

both_neq : (x = x' -> Void) -> (y = y' -> Void) -> (PairTy x y = PairTy x' y') -> Void
both_neq contra_x contra_y Refl = contra_x Refl


-- Decidablility --

DecEq Universe.Ty where
  decEq (ListTy a)  (ListTy b)   with (decEq a b)
    decEq (ListTy b)  (ListTy b) | (Yes Refl)                       = Yes Refl
    decEq (ListTy a)  (ListTy b) | (No contra)                      = No (list_neq contra)

  decEq (PairTy x y) (PairTy x' y')     with (decEq x x')
    decEq (PairTy x y) (PairTy x y')    | (Yes Refl)  with (decEq y y')
      decEq (PairTy x y) (PairTy x y)   | (Yes Refl)  | (Yes Refl)    = Yes Refl
      decEq (PairTy x y) (PairTy x y')  | (Yes Refl)  | (No contra)   = No (snd_neq contra)
    decEq (PairTy x y) (PairTy x' y')   | (No contra) with (decEq y y')
      decEq (PairTy x y) (PairTy x' y)  | (No contra) | (Yes Refl)    = No (fst_neq contra)
      decEq (PairTy x y) (PairTy x' y') | (No contra) | (No contra')  = No (both_neq contra contra')

  decEq (BasicTy a)  (BasicTy b)   with (decEq a b)
    decEq (BasicTy b)  (BasicTy b) | (Yes Refl)                     = Yes Refl
    decEq (BasicTy a)  (BasicTy b) | (No contra)                    = No (basic_neq contra)

  decEq (PairTy _ _) (ListTy _)                                     = No absurd
  decEq (ListTy _)   (PairTy _ _)                                   = No (negEqSym absurd)
  decEq (PairTy _ _) (BasicTy _)                                    = No absurd
  decEq (BasicTy _)  (PairTy _ _)                                   = No (negEqSym absurd)

  decEq (ListTy _)   (BasicTy _)                                    = No absurd
  decEq (BasicTy _)  (ListTy _)                                     = No (negEqSym absurd)
