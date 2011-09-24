module Net.Operator (operation) where

import Control.Concurrent
import Control.Monad.Trans

import Utility.Prim
import Utility.Message
import Net.Gate
import MetaData.Types

defaultWaitMicroSecond = 1000000

operation :: Procedure
operation = land >> release >> (lift $ threadDelay defaultWaitMicroSecond) >> operation

-- lift $ runGate procedure
land :: ClientThread ()
land = (lift $ runGate $ landLoop []) >>= \rs -> post (AI, NM $ show rs) >> post (UIOut, NM $ "output " ++ show rs)
    where
      landLoop :: [(Clip,Destination)] -> Gate [(Clip,Destination)]
      landLoop cs = pop >>= \recv -> case recv of
                                       Just cd -> landLoop (cd:cs)
                                       Nothing -> return cs

release :: ClientThread ()
release = gather [] >>= \cs -> lift $ runGate $ mapM_ push cs 
    where
      gather :: [Clip] -> ClientThread [Clip]
      gather cs = tryFetch >>= \req -> case req of
                                         Just (NM c) -> gather ((read c):cs) 
                                         Nothing     -> return cs  -- "End of the Loop"
