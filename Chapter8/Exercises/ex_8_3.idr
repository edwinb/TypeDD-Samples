data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys, zs

{- 1 -}

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
           (contra : (x = y) -> Void) -> (x :: xs) = (y :: ys) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
          (contra : (xs = ys) -> Void) -> (x :: xs) = (y :: ys) -> Void
tailUnequal contra Refl = contra Refl

{- 2 -}

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) = case decEq x y of
                                     No contra => No (headUnequal contra)
                                     Yes Refl => case decEq xs ys of
                                                      Yes Refl => Yes Refl
                                                      No contra => No (tailUnequal contra)
