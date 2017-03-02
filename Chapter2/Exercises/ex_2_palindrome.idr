module Main

palindrome : String -> Bool
palindrome str = let strL = toLower str in
                     strL == reverse strL

main : IO ()
main = repl "Enter a string: " show_palindrome
  where
    show_palindrome : String -> String
    show_palindrome x = show (palindrome x) ++ "\n"
    
