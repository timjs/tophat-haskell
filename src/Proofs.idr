module Proofs

%access public export
%default total

append_non_empty_both : (xs : List a) -> (ys : List a) -> NonEmpty xs -> NonEmpty ys -> NonEmpty (xs ++ ys)
append_non_empty_both (x :: xs) (y :: ys) IsNonEmpty IsNonEmpty = IsNonEmpty

-- (|+|) (Or xs {ok = ok_x}) (Or ys {ok = ok_y}) = Or (xs ++ ys) {ok = append_non_empty_both xs ys ok_x ok_y}
