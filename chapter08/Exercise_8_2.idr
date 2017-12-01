import Data.Vect

{- 1 -}

{-
`plusZeroRightNeutral` is proof of "m+0 = m", but we want proof of "m = m+0".
We can use `sym` which, given a proof of "left=right" returns a proof of
"right=left".
-}
plus_commutes_Z : m = plus m 0
plus_commutes_Z {m} = sym (plusZeroRightNeutral m)


{-
We need proof that "S (k+m) = m+(S k)".
First, we use plusCommutative to rewrite the left term:
"S (k+m)" -> "S (m+k)", resulting in "S (m+k) = m+(S k)".

We can then use plusSuccRightSucc (which proves "S (left+right) = left + (S right)")
which completes the proof.
-}
plus_commutes_S : (k : Nat) -> (m : Nat) -> S (plus k m) = plus m (S k)
plus_commutes_S k m = rewrite plusCommutative k m in
           rewrite plusSuccRightSucc m k in
           Refl

{-
Prove "n+m = m+n"
-}
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = plus_commutes_Z
myPlusCommutes (S k) m = plus_commutes_S k m

{- 2 -}

{-
"n = n+0" proof given by `plusZeroRightNeutral`, which can be used to prove
"Vect n a = Vect (n + 0) a".
-}
reverseProof_nil : (acc : Vect n a) -> Vect (plus n 0) a
reverseProof_nil {n} acc = rewrite plusZeroRightNeutral n in acc

{-
"(S n) + len" in acc simplifies to "S (plus n len)" immediately (how?).
`sym (plusSuccRightSucc n len)` proves that "n+(S len) = S (n+len)", which can
be used to rewrite the return type so that it's equal to the type of acc.
-}
reverseProof_xs : (acc : Vect ((S n) + len) a) -> Vect (plus n (S len)) a
reverseProof_xs {n} {len} acc = rewrite sym (plusSuccRightSucc n len) in acc

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)
