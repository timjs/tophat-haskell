module Helpers


import public Control.Catchable


%default total
%access export


infixl 3 <*>, <&>



-- Panic -----------------------------------------------------------------------


panic : String -> a
panic = idris_crash



-- List extensions -------------------------------------------------------------


replace : Nat -> a -> List a -> List a
replace Z y (_ :: xs) = y :: xs
replace n y (x :: xs) = x :: replace (pred n) y xs
replace _ _ []        = []


delete : Nat -> List a -> List a
delete Z (_ :: xs) = xs
delete n (x :: xs) = x :: delete (pred n) xs
delete _ []        = []



-- Applicative extensions ------------------------------------------------------


unit : Applicative f => f ()
unit = pure ()


(<&>) : Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = MkPair <$> x <*> y



-- Catchable extensions --------------------------------------------------------


ok : Applicative f => Catchable f e => a -> f a
ok = pure



-- String decomposition --------------------------------------------------------


strSnoc : String -> Char -> String
strSnoc s c = s ++ (singleton c)


between : Char -> Char -> String -> String
between a b str = strCons a (strSnoc str b)


partial
strLast : String -> Char
strLast str = strIndex str $ cast (length str) - 1


strMid : String -> String
strMid str = substr 1 (pred $ pred $ length str) str


mutual

  public export
  data Decons : String -> Type where
    Empty  : Decons ""
    Single : (char : Char) -> Decons (singleton char)
    Multi  : (first : Char) -> (last : Char) -> (rest : String) -> Decons (between first last rest)

  -- NOTE: we need `believe_me` because the operations are primitives in the end
  -- NOTE: e need `assert_total` because we know the string will not be empty by the match on `length`
  decons : (str : String) -> Decons str
  decons str with (length str)
    decons ""  | Z   = Empty
    decons str | S Z = believe_me $ Single (assert_total $ strHead str)
    decons str | n   = believe_me $ Multi (assert_total $ strHead str) (assert_total $ strLast str) (strMid str)



{-
test : String -> IO ()
test str with (decons str)
  test ""                        | Empty          = putStr $ "<empty>"
  test (singleton char)          | (Single char)      = putStr $ "<single " ++ (singleton char) ++ ">"
  test (between first last rest) | (Multi first last rest) = putStr $ singleton first ++ "<middle " ++ rest ++ ">" ++ singleton last
-}
