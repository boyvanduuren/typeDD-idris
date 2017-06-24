module Main

subtractNat : Nat -> Nat -> Nat
subtractNat a b = case isLTE b a of
  Yes _ => a - b
  No _ => 0

twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type
rotate : Shape -> Shape
turn_around: Shape -> Shape
turn_around = twice rotate

main : IO ()
main = printLn (subtractNat 6 4)
