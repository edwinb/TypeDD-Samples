import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

quiz : Stream Int -> (score : Nat) -> (questions : Nat) -> ConsoleIO (Nat, Nat)
quiz (num1 :: num2 :: nums) score questions
   = do PutStr ("Score so far: " ++ show score ++ " / " ++ show questions ++ "\n")
        PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
        answer <- GetLine
        if toLower answer == "quit" then Quit (score, questions) else
          if (cast answer == num1 * num2)
            then do PutStr "Correct!\n"
                    quiz nums (score + 1) (questions + 1)
            else do PutStr ("Wrong, the answer is " ++ show (num1 * num2) ++ "\n")
                    quiz nums score (questions + 1)

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do seed <- time
          Just (score, qs) <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
               | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score ++ " / " ++ show qs)

