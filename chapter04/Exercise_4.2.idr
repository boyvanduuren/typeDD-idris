import Data.Vect

data PowerSource = Petrol | Electric | Pedal

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Motorcycle: (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  ECar : (charge : Nat) -> Vehicle Electric
  Tram : (charge : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel: Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

vectTake : (n: Nat) -> Vect (n + m) elem -> Vect n elem
vectTake n xs = take n xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            (Just x) => Just ((index x xs) + (index x ys))
