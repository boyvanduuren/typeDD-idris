data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just eq => Just (sameS _ _ eq)

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat' Z Z = Just (Same 0)
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = (case checkEqNat' k j of
                                Nothing => Nothing
                                Just (Same j) => Just (Same (S j)))

checkEqNat'' : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat'' Z Z = Just (Same 0)
checkEqNat'' Z (S k) = Nothing
checkEqNat'' (S k) Z = Nothing
checkEqNat'' (S k) (S j) = do
    Same j <- checkEqNat'' k j
    Just (Same (S j))
                            
exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 Just (Same len) => Just input

checkEqNatRefl : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNatRefl Z Z = Just Refl
checkEqNatRefl Z (S k) = Nothing
checkEqNatRefl (S k) Z = Nothing
checkEqNatRefl (S k) (S j) = case checkEqNatRefl k j of
                                  Nothing => Nothing
                                  (Just x) => Just (cong x)