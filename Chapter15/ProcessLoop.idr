import System.Concurrency.Channels

data Message = Add Nat Nat

data MessagePID = MkMessage PID

data Process : Type -> Type where
     Request : MessagePID -> Message -> Process (Maybe Nat)
     Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
     Spawn : Process () -> Process (Maybe MessagePID)

     Loop : Inf (Process a) -> Process a
     Action : IO a -> Process a
     Pure : a -> Process a
     (>>=) : Process a -> (a -> Process b) -> Process b

public export
data Fuel = Dry | More (Lazy Fuel)

export partial
forever : Fuel
forever = More forever

total
run : Fuel -> Process t -> IO (Maybe t)
run fuel (Request (MkMessage process) msg)
          = do Just chan <- connect process
                    | _ => pure (Just Nothing)
               ok <- unsafeSend chan msg
               if ok then do Just x <- unsafeRecv Nat chan
                                  | Nothing => pure (Just Nothing)
                             pure (Just (Just x))
                     else pure (Just Nothing)
run fuel (Respond f)
          = do Just sender <- listen 1
                    | Nothing => pure (Just Nothing)
               Just msg <- unsafeRecv Message sender
                    | Nothing => pure (Just Nothing)
               Just res <- run fuel (f msg)
                    | Nothing => pure Nothing
               unsafeSend sender res
               pure (Just (Just msg))
run (More fuel) (Loop proc) = run fuel proc
run fuel (Spawn proc) = do Just pid <- spawn (do run fuel proc
                                                 pure ())
                                | Nothing => pure Nothing
                           pure (Just (Just (MkMessage pid)))
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure val) = pure (Just val)
run fuel (act >>= next) = do Just x <- run fuel act
                                  | Nothing => pure Nothing
                             run fuel (next x)
run Dry _ = pure Nothing

procAdder : Process ()
procAdder = do Respond (\msg => case msg of
                                     Add x y => Pure (x + y))
               Loop procAdder

procMain : Process ()
procMain = do Just adder_id <- Spawn procAdder
                   | Nothing => Action (putStrLn "Spawn failed")
              Just answer <- Request adder_id (Add 2 3)
                   | Nothing => Action (putStrLn "Request failed")
              Action (printLn answer)

partial
runProc : Process () -> IO ()
runProc proc = do run forever proc
                  pure ()

main : IO ()
main = runProc procMain
