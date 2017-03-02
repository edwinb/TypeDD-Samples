module Main

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

main : IO ()
main = repl "Enter a string: " show_counts
  where
    show_counts : String -> String
    show_counts x = show (counts x) ++ "\n"
