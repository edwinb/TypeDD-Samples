import Data.Vect

{- 1 -}

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

{- 2 -}

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

{- 3 -}

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

{- 4 -}

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs
