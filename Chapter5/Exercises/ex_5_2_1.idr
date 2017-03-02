import System

{- 1 -}

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (answer : Nat) -> IO ()
guess answer = do
  putStr ("Guess a number between 1 and 100: ")
  isNum <- readNumber
  case isNum of
       Nothing => do putStrLn "Invalid input"
                     guess answer
       Just userguess => if userguess < answer
                            then do putStrLn "Too low"
                                    guess answer
                            else if userguess > answer
                                 then do putStrLn "Too high"
                                         guess answer
                                 else putStrLn "Well done!"

{- 2 -}

main : IO ()
main = do t <- time
          guess (cast (t `mod` 101))
