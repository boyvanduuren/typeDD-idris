import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

my_list_map : (a -> b) -> List a -> List b
my_list_map f [] = []
my_list_map f (x :: xs) = f x :: my_list_map f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
