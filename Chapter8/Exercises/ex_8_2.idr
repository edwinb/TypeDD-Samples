import Data.Vect

{- 1 -}

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         rewrite plusSuccRightSucc m k in Refl

{- 2 -}

reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect (S n + k) a -> Vect (plus n (S k)) a
reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs)
                        = reverseProof_xs (reverse' (x::acc) xs)
