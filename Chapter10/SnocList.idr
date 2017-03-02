data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input} snoc (x :: xs)
    = rewrite appendAssociative input [x] xs in snocListHelp (Snoc snoc) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

my_reverse_help : (input : List a) -> SnocList input -> List a
my_reverse_help [] Empty = []
my_reverse_help (xs ++ [x]) (Snoc rec) = x :: my_reverse_help xs rec

my_reverse1 : List a -> List a
my_reverse1 input = my_reverse_help input (snocList input)

my_reverse : List a -> List a
my_reverse input with (snocList input)
  my_reverse [] | Empty = []
  my_reverse (xs ++ [x]) | (Snoc rec) = x :: my_reverse xs | rec
