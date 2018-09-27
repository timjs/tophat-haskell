||| These are just the same interface implementations as in Prelude.Interfaces,
||| but named, so we can refer to them and safe the dictionaries in data structures.
module Interfaces.Named


%default total
%access public export



[eqUnit] Eq () where
  () == () = True


[eqBool] Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False


[eqInt] Eq Int where
  (==) = boolOp prim__eqInt


[eqString] Eq String where
  (==) = boolOp prim__eqString


[eqList] Eq a => Eq (List a) where
  (==) []      []      = True
  (==) (x::xs) (y::ys) with ( x == y )
    | True             = xs == ys
    | False            = False
  (==) _ _             = False
