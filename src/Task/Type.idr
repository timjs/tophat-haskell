module Task.Type

import Data.String

%default total
%access public export


-- Type universe ---------------------------------------------------------------

data BasicTy
    = BoolTy
    | IntTy
    | StringTy

data Ty
    = UnitTy
    | PairTy Ty Ty
    | Basic BasicTy

typeOfBasic : BasicTy -> Type
typeOfBasic BoolTy   = Bool
typeOfBasic IntTy    = Int
typeOfBasic StringTy = String

||| Conversion of Task types to Idris types.
typeOf : Ty -> Type
typeOf UnitTy       = ()
typeOf (PairTy x y) = ( typeOf x, typeOf y )
typeOf (Basic b)    = typeOfBasic b


-- Lemmas ----------------------------------------------------------------------

Uninhabited (UnitTy = PairTy x y) where
    uninhabited Refl impossible

Uninhabited (UnitTy = Basic b) where
    uninhabited Refl impossible

Uninhabited (PairTy x y = Basic b) where
    uninhabited Refl impossible

Uninhabited (BoolTy = IntTy) where
    uninhabited Refl impossible

Uninhabited (BoolTy = StringTy) where
    uninhabited Refl impossible

Uninhabited (IntTy = StringTy) where
    uninhabited Refl impossible

basic_neq : (a = b -> Void) -> (Basic a = Basic b) -> Void
basic_neq contra Refl = contra Refl

snd_neq : (y = y' -> Void) -> (PairTy x y = PairTy x y') -> Void
snd_neq contra Refl = contra Refl

fst_neq : (x = x' -> Void) -> (PairTy x y = PairTy x' y) -> Void
fst_neq contra Refl = contra Refl

both_neq : (x = x' -> Void) -> (y = y' -> Void) -> (PairTy x y = PairTy x' y') -> Void
both_neq contra_x contra_y Refl = contra_x Refl


-- Decidablility ---------------------------------------------------------------

DecEq BasicTy where
    decEq BoolTy   BoolTy   = Yes Refl
    decEq IntTy    IntTy    = Yes Refl
    decEq StringTy StringTy = Yes Refl
    decEq BoolTy   IntTy    = No absurd
    decEq IntTy    BoolTy   = No (negEqSym absurd)
    decEq BoolTy   StringTy = No absurd
    decEq StringTy BoolTy   = No (negEqSym absurd)
    decEq IntTy    StringTy = No absurd
    decEq StringTy IntTy    = No (negEqSym absurd)

DecEq Ty where
    decEq UnitTy       UnitTy                                             = Yes Refl
    decEq (PairTy x y) (PairTy x' y')     with (decEq x x')
      decEq (PairTy x y) (PairTy x y')    | (Yes Refl)  with (decEq y y')
        decEq (PairTy x y) (PairTy x y)   | (Yes Refl)  | (Yes Refl)      = Yes Refl
        decEq (PairTy x y) (PairTy x y')  | (Yes Refl)  | (No contra)     = No (snd_neq contra)
      decEq (PairTy x y) (PairTy x' y')   | (No contra) with (decEq y y')
        decEq (PairTy x y) (PairTy x' y)  | (No contra) | (Yes Refl)      = No (fst_neq contra)
        decEq (PairTy x y) (PairTy x' y') | (No contra) | (No contra')    = No (both_neq contra contra')
    decEq (Basic a)  (Basic b)   with (decEq a b)
      decEq (Basic b)  (Basic b) | (Yes Refl)                             = Yes Refl
      decEq (Basic a)  (Basic b) | (No contra)                            = No (basic_neq contra)
    decEq UnitTy       (PairTy x y)                                       = No absurd
    decEq (PairTy x y) UnitTy                                             = No (negEqSym absurd)
    decEq UnitTy       (Basic b)                                          = No absurd
    decEq (Basic b)    UnitTy                                             = No (negEqSym absurd)
    decEq (PairTy x y) (Basic b)                                          = No absurd
    decEq (Basic b)    (PairTy x y)                                       = No (negEqSym absurd)


-- Parsing ---------------------------------------------------------------------

parse : String -> Maybe (b : BasicTy ** typeOfBasic b)
--FIXME: rewrite this using parser combinators from Text.Parser of Text.Lexer
parse "True"                  = Just $ (BoolTy ** True)
parse "False"                 = Just $ (BoolTy ** False)
parse s   with (the (Maybe Int) (parseInteger s))
  parse s | (Just int)        = Just $ (IntTy ** int)
  parse s | Nothing   with (isPrefixOf "\"" s && isSuffixOf "\"" s)
    parse s | Nothing | True  = Just $ (StringTy ** substr 1 (pred $ pred $ length s) s)
    parse s | Nothing | False = Nothing
