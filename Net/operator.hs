{-# LANGUAGE TupleSections #-}

module Net.Operator (operation, features) where

import Control.Concurrent
import Control.Monad.Trans

import Utility.Prim
import Utility.Message
import Net.Gate
import MetaData.Types

features :: [(String, Signature)] 
features = map (,Net) supports

supports :: [String]
supports = ["Add"]

defaultWaitMicroSecond = 1000000

operation :: Procedure
operation = lift initialState >>= operation'
    where
      operation' :: GateState -> Procedure
      operation' is = land is >>= release >>= \gs -> (lift $ threadDelay defaultWaitMicroSecond) >> operation' gs

-- lift $ runGate procedure
land :: GateState -> ClientThread GateState
land gs = (lift $ runGate (land' []) gs) >>= \(rs, ngs) -> post (AI, NM $ "Add " ++ show rs) >> post (UIOut, NM $ "output " ++ show rs) >> return ngs
    where
      land' :: [(Clip,Destination)] -> Gate [(Clip,Destination)]
      land' cs = pop >>= \recv -> land' $ recv ++ cs

release :: GateState -> ClientThread GateState
release gs = gather [] >>= \cs -> lift $ runGate (mapM_ push cs) gs >>= \(_, ngs) -> return ngs 
    where
      gather :: [Clip] -> ClientThread [Clip]
      gather cs = tryFetch >>= \req -> case req of 
                                         Just (NM c) -> let _:x:_ = words c in gather ((read x):cs) 
                                         Nothing     -> return cs  -- "End of the Loop"