data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

{- 1 -}

Eq Shape where
    (==) (Triangle base height) (Triangle base' height')
         = base == base' && height == height'
    (==) (Rectangle length height) (Rectangle length' height')
         = length == length' && height == height'
    (==) (Circle radius) (Circle radius') = radius == radius'
    (==) _ _ = False

{- 2 -}

Ord Shape where
    compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, 
              Rectangle 2 7]
