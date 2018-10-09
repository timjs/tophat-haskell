module Task.Universe


import Control.Monad.Ref

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



-- Primitive types -------------------------------------------------------------


data PrimitiveTy
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


DecEq PrimitiveTy where
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


Universe PrimitiveTy where
  typeOf UNIT   = ()
  typeOf BOOL   = Bool
  typeOf INT    = Int
  typeOf STRING = String

  show UNIT   = %implementation
  show BOOL   = %implementation
  show INT    = %implementation
  show STRING = %implementation

  eq UNIT   = %implementation
  eq BOOL   = %implementation
  eq INT    = %implementation
  eq STRING = %implementation



-- Full type universe ----------------------------------------------------------


data Ty
  = PAIR Ty Ty
  | LIST Ty
  | LOC Ty
  | PRIM PrimitiveTy



-- Lemmas --


Uninhabited (PAIR _ _ = LIST _) where
  uninhabited Refl impossible


Uninhabited (PAIR _ _ = LOC _) where
  uninhabited Refl impossible


Uninhabited (PAIR _ _ = PRIM _) where
  uninhabited Refl impossible


Uninhabited (LIST _ = LOC _) where
  uninhabited Refl impossible


Uninhabited (LIST _ = PRIM _) where
  uninhabited Refl impossible


Uninhabited (LOC _ = PRIM _) where
  uninhabited Refl impossible


private
list_neq : (a = b -> Void) -> (LIST a = LIST b) -> Void
list_neq contra Refl = contra Refl


private
loc_neq : (a = b -> Void) -> (LOC a = LOC b) -> Void
loc_neq contra Refl = contra Refl


private
prim_neq : (p = q -> Void) -> (PRIM p = PRIM q) -> Void
prim_neq contra Refl = contra Refl


private
snd_neq : (b = b' -> Void) -> (PAIR a b = PAIR a b') -> Void
snd_neq contra Refl = contra Refl


private
fst_neq : (a = a' -> Void) -> (PAIR a b = PAIR a' b) -> Void
fst_neq contra Refl = contra Refl


private
both_neq : (a = a' -> Void) -> (b = b' -> Void) -> (PAIR a b = PAIR a' b') -> Void
both_neq contra_a contra_b Refl = contra_a Refl



-- Decidablility --


DecEq Ty where
  decEq (PAIR a b) (PAIR a' b')     with (decEq a a')
    decEq (PAIR a b) (PAIR a b')    | (Yes Refl)  with (decEq b b')
      decEq (PAIR a b) (PAIR a b)   | (Yes Refl)  | (Yes Refl)    = Yes Refl
      decEq (PAIR a b) (PAIR a b')  | (Yes Refl)  | (No contra)   = No (snd_neq contra)
    decEq (PAIR a b) (PAIR a' b')   | (No contra) with (decEq b b')
      decEq (PAIR a b) (PAIR a' b)  | (No contra) | (Yes Refl)    = No (fst_neq contra)
      decEq (PAIR a b) (PAIR a' b') | (No contra) | (No contra')  = No (both_neq contra contra')

  decEq (LIST a)  (LIST b)   with (decEq a b)
    decEq (LIST b)  (LIST b) | (Yes Refl)                         = Yes Refl
    decEq (LIST a)  (LIST b) | (No contra)                        = No (list_neq contra)

  decEq (LOC a)  (LOC b)   with (decEq a b)
    decEq (LOC b)  (LOC b) | (Yes Refl)                           = Yes Refl
    decEq (LOC a)  (LOC b) | (No contra)                          = No (loc_neq contra)

  decEq (PRIM p)  (PRIM q)   with (decEq p q)
    decEq (PRIM q)  (PRIM q) | (Yes Refl)                         = Yes Refl
    decEq (PRIM p)  (PRIM q) | (No contra)                        = No (prim_neq contra)

  decEq (PAIR _ _) (LIST _)                                       = No absurd
  decEq (LIST _)   (PAIR _ _)                                     = No (negEqSym absurd)
  decEq (PAIR _ _) (LOC _)                                        = No absurd
  decEq (LOC _)    (PAIR _ _)                                     = No (negEqSym absurd)
  decEq (PAIR _ _) (PRIM _)                                       = No absurd
  decEq (PRIM _)   (PAIR _ _)                                     = No (negEqSym absurd)

  decEq (LIST _)   (LOC _)                                        = No absurd
  decEq (LOC _)    (LIST _)                                       = No (negEqSym absurd)
  decEq (LIST _)   (PRIM _)                                       = No absurd
  decEq (PRIM _)   (LIST _)                                       = No (negEqSym absurd)

  decEq (LOC _)    (PRIM _)                                       = No absurd
  decEq (PRIM _)   (LOC _)                                        = No (negEqSym absurd)


Eq (IORef a) where
  (==) r1 r2 = False


Show (IORef a) where
  show ref = "<ref>"


Universe Ty where
  typeOf (PAIR a b) = ( typeOf a, typeOf b )
  typeOf (LIST a)   = List (typeOf a)
  typeOf (LOC a)    = IORef (typeOf a) --FIXME: is this ok??? Should be parametrised over `MonadRef l m`...
  typeOf (PRIM p)   = typeOf p

  show (PAIR x y) with ( show x, show y )
    | ( show_x, show_y ) = %implementation
  show (LIST x) with ( show x )
    | show_x = %implementation
  show (LOC x) with ( show x )
    | show_x = %implementation
  show (PRIM x) with ( show x )
    | show_x = %implementation

  eq (PAIR x y) with ( eq x, eq y )
    | ( eq_x, eq_y ) = %implementation
  eq (LIST x) with ( eq x )
    | eq_x = %implementation
  eq (LOC x) with ( eq x )
    | eq_x = %implementation
  eq (PRIM x) with ( eq x )
    | eq_x = %implementation



-- Parsing --


parse : String -> Maybe (b : Ty ** ( typeOf b ))
parse "()"                                                        = Just (PRIM UNIT ** ())
parse "True"                                                      = Just (PRIM BOOL ** True)
parse "False"                                                     = Just (PRIM BOOL ** False)
parse s   with (the (Maybe Int) (parseInteger s))
  parse s | Just int                                              = Just (PRIM INT ** int)
  parse s | Nothing                        with (decons s)
    parse (between '"' '"' rest) | Nothing | (Multi '"' '"' rest) = Just (PRIM STRING ** rest)
    parse _                      | Nothing | _                    = Nothing

--FIXME: add pairs and lists, but no locs
