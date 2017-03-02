{- 4 -}

my_repl : (prompt : String) ->
          (fn : String -> String) -> IO ()
my_repl prompt fn
   = do putStr prompt
        x <- getLine
        putStr (fn x)
        my_repl prompt fn

my_replWith : (state : a) -> (prompt : String) ->
              (fn : a -> String -> Maybe (String, a)) -> IO ()
my_replWith acc prompt fn
   = do putStr prompt
        x <- getLine
        case fn acc x of
             Just (out, acc') => do putStr out
                                    my_replWith acc' prompt fn
             Nothing => pure ()
