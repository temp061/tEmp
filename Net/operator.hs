module Net.Operator (operation) where

import Control.Concurrent
import Control.Monad.Trans

import Utility.Prim
import Utility.Message
import Net.Gate
import MetaData.Types

defaultWaitMicroSecond = 1000000

operation :: Procedure
operation = lift initialState >>= operation'
    where
      operation' :: GateState -> Procedure
      operation' is = land is >>= release >>= \gs -> (lift $ threadDelay defaultWaitMicroSecond) >> operation' gs

-- lift $ runGate procedure
land :: GateState -> ClientThread GateState
land gs = (lift $ runGate (land' []) gs) >>= \(rs, ngs) -> post (AI, NM $ show rs) >> post (UIOut, NM $ "output " ++ show rs) >> return ngs
    where
      land' :: [(Clip,Destination)] -> Gate [(Clip,Destination)]
      land' cs = pop >>= \recv -> case recv of
                                    Just cd -> land' (cd:cs)
                                    Nothing -> return cs

release :: GateState -> ClientThread GateState
release gs = gather [] >>= \cs -> lift $ runGate (mapM_ push cs) gs >>= \(_, ngs) -> return ngs 
    where
      gather :: [Clip] -> ClientThread [Clip]
      gather cs = tryFetch >>= \req -> case req of
                                         Just (NM c) -> gather ((read c):cs) 
                                         Nothing     -> return cs  -- "End of the Loop"
