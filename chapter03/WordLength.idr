allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

xor : Bool -> Bool -> Bool
xor False y = ?Bool_rhs_1
xor True y = ?Bool_rhs_2

foo: Nat -> Nat
foo k = ?foo_rhs
