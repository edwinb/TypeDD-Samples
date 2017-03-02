import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" 
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

{- 1 -}

update : (stateType -> stateType) -> State stateType ()
update f = do st <- get
              put (f st)

increment : Nat -> State Nat ()
increment x = update (+x)

{- 2 -}

countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left val right) = do countEmpty left
                                      countEmpty right

{- 3 -}

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (empty, nodes) <- get
                          put (empty + 1, nodes)
countEmptyNode (Node left val right) = do countEmptyNode left
                                          (empty, nodes) <- get
                                          put (empty, nodes + 1)
                                          countEmptyNode right



