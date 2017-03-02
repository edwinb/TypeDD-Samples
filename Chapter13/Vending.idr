VendState : Type
VendState = (Nat, Nat)

data Input = COIN 
           | VEND 
           | CHANGE 
           | REFILL Nat

strToInput : String -> Maybe Input
strToInput "insert" = Just COIN
strToInput "vend" = Just VEND
strToInput "change" = Just CHANGE
strToInput x = if all isDigit (unpack x)
                  then Just (REFILL (cast x))
                  else Nothing

data MachineCmd : Type -> VendState -> VendState -> Type where
     InsertCoin : MachineCmd () (pounds, chocs)     (S pounds, chocs)
     Vend       : MachineCmd () (S pounds, S chocs) (pounds, chocs)
     GetCoins   : MachineCmd () (pounds, chocs)     (Z, chocs)

     Display : String -> 
                  MachineCmd () state               state
     Refill : (bars : Nat) -> 
                  MachineCmd () (Z, chocs)          (Z, bars + chocs)

     GetInput : MachineCmd (Maybe Input) state state 

     Pure : ty -> MachineCmd ty state state
     (>>=) : MachineCmd a state1 state2 -> (a -> MachineCmd b state2 state3) ->
             MachineCmd b state1 state3

data MachineIO : VendState -> Type where
     Do : MachineCmd a state1 state2 ->
          (a -> Inf (MachineIO state2)) -> MachineIO state1

runMachine : MachineCmd ty inState outState -> IO ty
runMachine InsertCoin = putStrLn "Coin inserted"
runMachine Vend = putStrLn "Please take your chocolate"
runMachine {inState = (pounds, _)} GetCoins 
     = putStrLn (show pounds ++ " coins returned")
runMachine (Display str) = putStrLn str
runMachine (Refill bars)
     = putStrLn ("Chocolate remaining: " ++ show bars)
runMachine {inState = (pounds, chocs)} GetInput
     = do putStrLn ("Coins: " ++ show pounds ++ "; " ++
                    "Stock: " ++ show chocs)
          putStr "> "
          x <- getLine
          pure (strToInput x)
runMachine (Pure x) = pure x
runMachine (cmd >>= prog) = do x <- runMachine cmd
                               runMachine (prog x)

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> MachineIO state -> IO ()
run (More fuel) (Do c f) 
     = do res <- runMachine c
          run fuel (f res)
run Dry p = pure ()


namespace MachineDo
  (>>=) : MachineCmd a state1 state2 ->
          (a -> Inf (MachineIO state2)) -> MachineIO state1
  (>>=) = Do

mutual
  vend : MachineIO (pounds, chocs)
  vend {pounds = S p} {chocs = S c} = do Vend
                                         Display "Enjoy!"
                                         machineLoop
  vend {pounds = Z} = do Display "Insert a coin"
                         machineLoop
  vend {chocs = Z} = do Display "Out of stock"
                        machineLoop

  refill : (num : Nat) -> MachineIO (pounds, chocs)
  refill {pounds = Z} num = do Refill num
                               machineLoop
  refill _ = do Display "Can't refill: Coins in machine"
                machineLoop

  machineLoop : MachineIO (pounds, chocs)
  machineLoop =
       do Just x <- GetInput | Nothig => do Display "Invalid input"
                                            machineLoop
          case x of
              COIN => do InsertCoin
                         machineLoop
              VEND => vend
              CHANGE => do GetCoins
                           Display "Change returned"
                           machineLoop
              REFILL num => refill num

main : IO ()
main = run forever (machineLoop {pounds = 0} {chocs = 1})

