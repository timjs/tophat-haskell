module Helpers

%default total
%access export

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
  test ""                       | Empty                    = putStr $ "<empty>"
  test (singleton char)         | (Single char)            = putStr $ "<single " ++ (singleton char) ++ ">"
  test (between first last rest) | (Multi first last rest) = putStr $ singleton first ++ "<middle " ++ rest ++ ">" ++ singleton last
-}
