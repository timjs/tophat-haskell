module Task.Universe

import Data.String
import public Data.Universe

import Helpers

%default total
%access public export


-- Labels ----------------------------------------------------------------------

Label : Type
Label = String

isLabel : String -> Bool
isLabel s               with ( strM s )
 isLabel ""             | StrNil      = False
 isLabel (strCons c cs) | StrCons c _ = isUpper c


-- Basic universe --------------------------------------------------------------

data BasicTy
  = UNIT
  | BOOL
  | INT
  | STRING


-- Lemmas --

Uninhabited (UNIT = BOOL) where
  uninhabited Refl impossible

Uninhabited (UNIT = INT) where
  uninhabited Refl impossible

Uninhabited (UNIT = STRING) where
  uninhabited Refl impossible

Uninhabited (BOOL = INT) where
  uninhabited Refl impossible

Uninhabited (BOOL = STRING) where
  uninhabited Refl impossible

Uninhabited (INT = STRING) where
  uninhabited Refl impossible


-- Universe --

DecEq BasicTy where
  decEq UNIT   UNIT   = Yes Refl
  decEq BOOL   BOOL   = Yes Refl
  decEq INT    INT    = Yes Refl
  decEq STRING STRING = Yes Refl
  decEq UNIT   BOOL   = No absurd
  decEq BOOL   UNIT   = No (negEqSym absurd)
  decEq UNIT   INT    = No absurd
  decEq INT    UNIT   = No (negEqSym absurd)
  decEq UNIT   STRING = No absurd
  decEq STRING UNIT   = No (negEqSym absurd)
  decEq BOOL   INT    = No absurd
  decEq INT    BOOL   = No (negEqSym absurd)
  decEq BOOL   STRING = No absurd
  decEq STRING BOOL   = No (negEqSym absurd)
  decEq INT    STRING = No absurd
  decEq STRING INT    = No (negEqSym absurd)

Universe BasicTy where
  typeOf UNIT   = ()
  typeOf BOOL   = Bool
  typeOf INT    = Int
  typeOf STRING = String

  defaultOf UNIT   = ()
  defaultOf BOOL   = False
  defaultOf INT    = 0
  defaultOf STRING = ""


-- Parsing --

parse : String -> Maybe (b : BasicTy ** typeOf b)
parse "()"                                                        = Just $ (UNIT ** ())
parse "True"                                                      = Just $ (BOOL ** True)
parse "False"                                                     = Just $ (BOOL ** False)
parse s   with (the (Maybe Int) (parseInteger s))
  parse s | (Just int)                                            = Just $ (INT ** int)
  parse s | Nothing                        with (decons s)
    parse (between '"' '"' rest) | Nothing | (Multi '"' '"' rest) = Just $ (STRING ** rest)
    parse _                      | Nothing | _                    = Nothing


-- Full universe ---------------------------------------------------------------

data Ty
  = PAIR Ty Ty
  | LIST Ty
  | BASIC BasicTy


-- Lemmas --

Uninhabited (PAIR _ _ = LIST _) where
  uninhabited Refl impossible

Uninhabited (PAIR _ _ = BASIC _) where
  uninhabited Refl impossible

Uninhabited (LIST _ = BASIC _) where
  uninhabited Refl impossible

private
list_neq : (a = b -> Void) -> (LIST a = LIST b) -> Void
list_neq contra Refl = contra Refl

private
basic_neq : (a = b -> Void) -> (BASIC a = BASIC b) -> Void
basic_neq contra Refl = contra Refl

private
snd_neq : (y = y' -> Void) -> (PAIR x y = PAIR x y') -> Void
snd_neq contra Refl = contra Refl

private
fst_neq : (x = x' -> Void) -> (PAIR x y = PAIR x' y) -> Void
fst_neq contra Refl = contra Refl

private
both_neq : (x = x' -> Void) -> (y = y' -> Void) -> (PAIR x y = PAIR x' y') -> Void
both_neq contra_x contra_y Refl = contra_x Refl


-- Decidablility --

DecEq Ty where
  decEq (LIST a)  (LIST b)   with (decEq a b)
    decEq (LIST b)  (LIST b) | (Yes Refl)                       = Yes Refl
    decEq (LIST a)  (LIST b) | (No contra)                      = No (list_neq contra)

  decEq (PAIR x y) (PAIR x' y')     with (decEq x x')
    decEq (PAIR x y) (PAIR x y')    | (Yes Refl)  with (decEq y y')
      decEq (PAIR x y) (PAIR x y)   | (Yes Refl)  | (Yes Refl)    = Yes Refl
      decEq (PAIR x y) (PAIR x y')  | (Yes Refl)  | (No contra)   = No (snd_neq contra)
    decEq (PAIR x y) (PAIR x' y')   | (No contra) with (decEq y y')
      decEq (PAIR x y) (PAIR x' y)  | (No contra) | (Yes Refl)    = No (fst_neq contra)
      decEq (PAIR x y) (PAIR x' y') | (No contra) | (No contra')  = No (both_neq contra contra')

  decEq (BASIC a)  (BASIC b)   with (decEq a b)
    decEq (BASIC b)  (BASIC b) | (Yes Refl)                     = Yes Refl
    decEq (BASIC a)  (BASIC b) | (No contra)                    = No (basic_neq contra)

  decEq (PAIR _ _) (LIST _)                                     = No absurd
  decEq (LIST _)   (PAIR _ _)                                   = No (negEqSym absurd)
  decEq (PAIR _ _) (BASIC _)                                    = No absurd
  decEq (BASIC _)  (PAIR _ _)                                   = No (negEqSym absurd)

  decEq (LIST _)   (BASIC _)                                    = No absurd
  decEq (BASIC _)  (LIST _)                                     = No (negEqSym absurd)

Universe Ty where
  typeOf (PAIR x y) = ( typeOf x, typeOf y )
  typeOf (LIST x)   = List (typeOf x)
  typeOf (BASIC b)  = typeOf b

  defaultOf (PAIR x y) = ( defaultOf x, defaultOf y )
  defaultOf (LIST x)   = []
  defaultOf (BASIC b)  = defaultOf b
