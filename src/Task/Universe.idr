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


Universe Ty where
  typeOf (PAIR a b) = ( typeOf a, typeOf b )
  typeOf (LIST a)   = List (typeOf a)
  typeOf (LOC a)    = IORef (typeOf a) --FIXME: is this ok??? Should be parametrised over `MonadRef l m`...
  typeOf (PRIM p)   = typeOf p



-- Basic types -----------------------------------------------------------------


data IsBasic : Ty -> Type where
  BasicPrim : IsBasic (PRIM p)
  BasicPair : IsBasic a -> IsBasic b -> IsBasic (PAIR a b)
  BasicList : IsBasic a -> IsBasic (LIST a)


private
loc_not_basic : IsBasic (LOC a) -> Void
loc_not_basic (BasicPrim)     impossible
loc_not_basic (BasicPair _ _) impossible
loc_not_basic (BasicList _)   impossible


private
list_not_basic : (IsBasic a -> Void) -> IsBasic (LIST a) -> Void
list_not_basic contra (BasicList prf) = contra prf


private
yes_no_pair_not_basic : (IsBasic b -> Void) -> IsBasic (PAIR a b) -> Void
yes_no_pair_not_basic contra (BasicPair _ prf_r) = contra prf_r


private
no_yes_pair_not_basic : (IsBasic a -> Void) -> IsBasic (PAIR a b) -> Void
no_yes_pair_not_basic contra (BasicPair prf_l _) = contra prf_l


private
no_no_pair_not_basic : (IsBasic a -> Void) -> (IsBasic b -> Void) -> IsBasic (PAIR a b) -> Void
no_no_pair_not_basic contra_l _ (BasicPair prf_l _) = contra_l prf_l


export
isBasic : (t : Ty) -> Dec (IsBasic t)
isBasic (PAIR a b) with ( isBasic a, isBasic b )
  | ( Yes l, Yes r ) = Yes $ BasicPair l r
  | ( Yes _, No ct ) = No $ yes_no_pair_not_basic ct
  | ( No ct, Yes _ ) = No $ no_yes_pair_not_basic ct
  | ( No c,  No d  ) = No $ no_no_pair_not_basic c d
isBasic (LIST a) with ( isBasic a )
  | Yes prf              = Yes $ BasicList prf
  | No contra            = No $ list_not_basic contra
isBasic (LOC _)          = No loc_not_basic
isBasic (PRIM p)         = Yes BasicPrim



-- Parsing --


parse : String -> Maybe (b : Ty ** ( IsBasic b, typeOf b ))
parse "()"                                                        = Just $ (PRIM UNIT ** ( BasicPrim, () ))
parse "True"                                                      = Just $ (PRIM BOOL ** ( BasicPrim, True ))
parse "False"                                                     = Just $ (PRIM BOOL ** ( BasicPrim, False ))
parse s   with (the (Maybe Int) (parseInteger s))
  parse s | (Just int)                                            = Just $ (PRIM INT ** ( BasicPrim, int ))
  parse s | Nothing                        with (decons s)
    parse (between '"' '"' rest) | Nothing | (Multi '"' '"' rest) = Just $ (PRIM STRING ** ( BasicPrim, rest ))
    parse _                      | Nothing | _                    = Nothing
--FIXME: add pairs and lists


-- Defaults --


export
defaultOf : (b : Ty) -> {auto p : IsBasic b} -> typeOf b
defaultOf (PRIM UNIT)   = ()
defaultOf (PRIM BOOL)   = False
defaultOf (PRIM INT)    = 0
defaultOf (PRIM STRING) = ""
defaultOf (LIST _)      = []
defaultOf (PAIR a b) with ( isBasic a, isBasic b )
  | ( Yes l, Yes r )    = ( defaultOf a, defaultOf b )
  --FIXME: what to do with No?
