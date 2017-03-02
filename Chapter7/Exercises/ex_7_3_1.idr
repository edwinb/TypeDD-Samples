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

Functor Expr where
    map f (Val x) = Val (f x)
    map f (Add x y) = Add (map f x) (map f y)
    map f (Sub x y) = Sub (map f x) (map f y)
    map f (Mul x y) = Mul (map f x) (map f y)
    map f (Div x y) = Div (map f x) (map f y)
    map f (Abs x) = Abs (map f x)
