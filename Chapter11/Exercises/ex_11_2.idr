{- Preamble, solution below -}

import Data.Primitives.Views
import System

%default total

data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = putStrLn "Out of fuel"

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

partial
forever : Fuel
forever = More forever

{- Solution -}

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = 
   do putStr prompt
      inp <- getLine
      putStr (action inp)
      totalREPL prompt action

