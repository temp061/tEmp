module Net.Operator (operation) where

{-
*** operation ***
push   -- from AI
kick   -- from AI
reject -- from UI
-}

opMap = Map.fromList [(Push, push), (Kick, kick), (Reject, reject)]

operation :: Procedure
operation = do ch <- lift $ forkGate procedure []
               handle ch

procedure :: Gate ()
procedure = forever do (GateState ch _ _) <- get
                       isEmpty <- lift $ atomically $ isEmptyTChan ch
                       if isEmpty
                       then
                           clipDestList <- pop
                           lift $ atomically $ mapM_ (\(c,d) -> writeTChan ch $ GM "NewClip" c d ) clipDestList
                       else
                           gm <- lift $ atomically $ readTChan ch
                           case order gm of
                             "Push"   -> push   $ clip gm 
                             "Kick"   -> kick   $ dest gm
                             "Reject" -> reject $ dest gm

handle :: TChan GateMessage -> CliantThread ()
handle ch = fetch >>= handleOn ch >> handle ch

handleOn ch msg = 
