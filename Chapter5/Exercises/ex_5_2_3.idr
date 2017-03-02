import System

{- 3 -}

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStrLn (show guesses ++ " guesses so far")
  putStr ("Guess a number between 1 and 100: ")
  isNum <- readNumber
  case isNum of
       Nothing => do putStrLn "Invalid input"
                     guess target guesses
       Just userguess => if userguess < target
                            then do putStrLn "Too low"
                                    guess target (guesses + 1)
                            else if userguess > target
                                 then do putStrLn "Too high"
                                         guess target  (guesses + 1)
                                 else putStrLn "Well done!"

main : IO ()
main = do t <- time
          guess (cast (t `mod` 101)) 0
