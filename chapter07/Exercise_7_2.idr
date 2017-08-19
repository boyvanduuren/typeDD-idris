{- copied from Expr.idr -}
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

{- 1 -}
Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " % " ++ show y ++ ")"
  show (Abs x) = "abs(" ++ show x ++ ")"

{- 2 -}
(Eq ty, Neg ty, Integral ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

{- 3 -}
(Neg ty, Integral ty) => Cast (Expr ty) ty where
  cast orig = eval orig
