{-
  メインフレーム：
  メッセージを受け取り、各モジュールに渡すだけを行う
-}

module Main (main) where

import qualified Data.Map as Map
import Control.Concurrent
import Utility.Prim
import MetaData.Binder

import qualified UI.InputOperator as UIIn
import qualified UI.OutputOperator as UIOut
import qualified MetaData.Operator as MetaData
import qualified Net.Operator as Net
import qualified AI.Operator as AI

procedures :: [(Signature, Procedure, ClientState)] 
procedures = [(UIIn, UIIn.operation, UnitState), (UIOut, UIOut.operation, UnitState), (MetaData, MetaData.operation, CS MetaData.Binder.empty), (Net, Net.operation, UnitState), (AI, AI.operation, UnitState)]

main :: IO ()
main = runMT (threadManager procedures) () Map.empty

threadManager :: [(Signature, Procedure, ClientState)] -> DispatcherThread ()
threadManager wps = do wl <- fork wps
                       setStatus $ Map.fromList wl
                       dispatch

dispatch :: DispatcherThread ()
dispatch = do (sig, m) <- fetch
              sigm <- getStatus
              case Map.lookup sig sigm of
                Nothing -> dispatch
                Just tid -> 
                    do require tid m
                       if isExit m 
                       then do killMThread tid -- threadの死に方は検討課題
                               let sigm' = Map.delete sig sigm
                               if Map.null sigm' 
                               then return ()
                               else setStatus sigm' >> dispatch
                       else dispatch
    where
      isExit (NM m) = m == "exit"

fork :: [(Signature, Procedure, ClientState)] 
     -> DispatcherThread [(Signature, ThreadId)]
fork [] = return []
fork ((sig, proc, initial):wps) = do tid <- forkMT initial proc
                                     ths <- fork wps
                                     return $ (sig, tid):ths
