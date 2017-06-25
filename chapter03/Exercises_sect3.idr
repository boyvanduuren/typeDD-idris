import Data.Vect

-- exercise 1
transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xTrans = transposeMat xs in
                             zipWith (::) x xTrans

-- exercise 2
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) ->
                     Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

-- exercise 3
multVectWithMatrix : Num a => (x : Vect m a) ->
                              (ys_trans : Vect p (Vect m a)) -> Vect p a
multVectWithMatrix x [] = []
multVectWithMatrix x (y :: ys) = sum (zipWith (*) x y) ::
                                     multVectWithMatrix x ys

multMatrix_helper : Num a => (xs : Vect n (Vect m a)) ->
                             (ys_trans : Vect p (Vect m a)) -> Vect n (Vect p a)
multMatrix_helper [] ys_trans = []
multMatrix_helper (x :: xs) ys_trans =
  multVectWithMatrix x ys_trans :: multMatrix_helper xs ys_trans

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) ->
                      Vect n (Vect p a)
multMatrix xs ys = let ys_trans = transposeMat ys in
                       multMatrix_helper xs ys_trans
