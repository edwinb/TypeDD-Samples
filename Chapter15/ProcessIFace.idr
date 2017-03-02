import System.Concurrency.Channels

data Message = Add Nat Nat

AdderType : Message -> Type
AdderType (Add x y) = Nat

data ListAction : Type where
     Length : List a -> ListAction
     Append : List a -> List a -> ListAction

ListType : ListAction -> Type
ListType (Length xs) = Nat
ListType (Append {a} xs ys) = List a

data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

NoRecv : Void -> Type
NoRecv = const Void

data ProcState = Ready | Sent | Looping

data Process : (iface : reqType -> Type) ->
               Type -> ProcState -> ProcState -> Type where
     Request : MessagePID service_iface ->
               (msg : service_reqType) ->
               Process iface (service_iface msg) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) Ready Ready) ->
               Process iface (Maybe reqType) st Sent
     Spawn : Process service_iface () Ready Looping ->
             Process iface (Maybe (MessagePID service_iface)) st st

     Loop : Inf (Process iface a Ready Looping) ->
            Process iface a Sent Looping
     Action : IO a -> Process iface a st st
     Pure : a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) ->
             Process iface b st1 st3

public export
data Fuel = Dry | More (Lazy Fuel)

export partial
forever : Fuel
forever = More forever

total
run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run fuel (Request {service_iface} (MkMessage process) msg)
          = do Just chan <- connect process
                    | _ => pure Nothing
               ok <- unsafeSend chan msg
               if ok then do Just x <- unsafeRecv (service_iface msg) chan
                                  | Nothing => pure Nothing
                             pure (Just x)
                     else pure Nothing
run fuel (Respond {reqType} calc)
          = do Just sender <- listen 1
                    | Nothing => pure (Just Nothing)
               Just msg <- unsafeRecv reqType sender
                    | Nothing => pure (Just Nothing)
               Just res <- run fuel (calc msg)
                    | Nothing => pure Nothing
               unsafeSend sender res
               pure (Just (Just msg))
run (More fuel) (Loop proc) = run fuel proc
run fuel (Spawn proc) = do Just pid <- spawn (do run fuel proc
                                                 pure ())
                                | Nothing => pure (Just Nothing)
                           pure (Just (Just (MkMessage pid)))
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure val) = pure (Just val)
run fuel (act >>= next) = do Just x <- run fuel act
                                  | Nothing => pure Nothing
                             run fuel (next x)
run Dry _ = pure Nothing

Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a Ready Looping

Client : Type -> Type
Client a = Process NoRecv a Ready Ready

{-
procAdderBad1 : Process () Ready Looping
procAdderBad1 = do Action (putStrLn "I'm out of the office today")
                   Loop procAdder_bad1

procAdderBad2 : Process () Ready Looping
procAdderBad2 = Loop procAdder_bad2
                    -}

procAdder : Service AdderType ()
procAdder = do Respond (\msg => case msg of
                                     Add x y => Pure (x + y))
               Loop procAdder

procMain : Client ()
procMain = do Just adder_id <- Spawn procAdder
                   | Nothing => Action (putStrLn "Spawn failed")
              answer <- Request adder_id (Add 2 3)
              Action (printLn answer)

partial
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do run forever proc
                  pure ()

main : IO ()
main = runProc procMain
