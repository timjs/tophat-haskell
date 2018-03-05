module Task.Type

%default total
%access public export


-- Type universe ---------------------------------------------------------------

data Ty
    = UNIT
    | INT
    | STRING
    | PAIR Ty Ty

||| Conversion of Task types to Idris types.
typeOf : Ty -> Type
typeOf UNIT       = ()
typeOf INT        = Int
typeOf STRING     = String
typeOf (PAIR a b) = ( typeOf a, typeOf b )


-- Lemmas ----------------------------------------------------------------------

Uninhabited (UNIT = INT) where
    uninhabited Refl impossible

Uninhabited (UNIT = STRING) where
    uninhabited Refl impossible

Uninhabited (UNIT = PAIR x y) where
    uninhabited Refl impossible

Uninhabited (INT = STRING) where
    uninhabited Refl impossible

Uninhabited (INT = PAIR x y) where
    uninhabited Refl impossible

Uninhabited (STRING = PAIR x y) where
    uninhabited Refl impossible

snd_neq : (y = y' -> Void) -> (PAIR x y = PAIR x y') -> Void
snd_neq contra Refl = contra Refl

fst_neq : (x = x' -> Void) -> (PAIR x y = PAIR x' y) -> Void
fst_neq contra Refl = contra Refl

both_neq : (x = x' -> Void) -> (y = y' -> Void) -> (PAIR x y = PAIR x' y') -> Void
both_neq contra_x contra_y Refl = contra_x Refl


-- Decidablility ---------------------------------------------------------------

DecEq Ty where
    decEq UNIT       UNIT                                             = Yes Refl
    decEq INT        INT                                              = Yes Refl
    decEq STRING     STRING                                           = Yes Refl
    decEq (PAIR x y) (PAIR x' y')     with (decEq x x')
      decEq (PAIR x y) (PAIR x y')    | (Yes Refl)  with (decEq y y')
        decEq (PAIR x y) (PAIR x y)   | (Yes Refl)  | (Yes Refl)      = Yes Refl
        decEq (PAIR x y) (PAIR x y')  | (Yes Refl)  | (No contra)     = No (snd_neq contra)
      decEq (PAIR x y) (PAIR x' y')   | (No contra) with (decEq y y')
        decEq (PAIR x y) (PAIR x' y)  | (No contra) | (Yes Refl)      = No (fst_neq contra)
        decEq (PAIR x y) (PAIR x' y') | (No contra) | (No contra')    = No (both_neq contra contra')
    decEq UNIT       INT                                              = No absurd
    decEq INT        UNIT                                             = No (negEqSym absurd)
    decEq UNIT       STRING                                           = No absurd
    decEq STRING     UNIT                                             = No (negEqSym absurd)
    decEq UNIT       (PAIR x y)                                       = No absurd
    decEq (PAIR x y) UNIT                                             = No (negEqSym absurd)
    decEq INT        STRING                                           = No absurd
    decEq STRING     INT                                              = No (negEqSym absurd)
    decEq INT        (PAIR x y)                                       = No absurd
    decEq (PAIR x y) INT                                              = No (negEqSym absurd)
    decEq STRING     (PAIR x y)                                       = No absurd
    decEq (PAIR x y) STRING                                           = No (negEqSym absurd)
