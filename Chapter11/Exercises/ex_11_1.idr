import Data.Primitives.Views

{- 1 -}

every_other : Stream a -> Stream a
every_other (x :: y :: xs) = y :: every_other xs

{- 2 -}

data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

Functor InfList where
    map func (value :: xs) = func value :: map func xs

{- 3 -}

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

data Face = Heads | Tails

total
getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf)
       = case rem of
              0 => Heads
              _ => Tails

coinFlips : Nat -> Stream Int -> List Face
coinFlips k rnds = map getFace (take k rnds)

{- 4 -}

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx
    = let next = (approx + (number / approx)) / 2 in
          approx :: square_root_approx number next

{- 5 -}

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound Z number bound (x :: xs) = x
square_root_bound (S k) number bound (x :: xs) =
       if (abs (x * x - number) < bound)
          then x
          else square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                       (square_root_approx number number)
