{-
  メインフレーム：
  メッセージを受け取り、各モジュールに渡すだけを行う
-}

module Main (main) where

import qualified Data.Map as Map
import Control.Concurrent
import Utility.Prim
import UI.CUI.Default
import MetaData.Types
import MetaData.Operator

procedures :: [(Signature, Procedure, ClientState)] 
procedures = [(UI, cuiProcedure, UnitState), (MetaData, operation, CS emptyBinder)]

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

{-
dispatch :: DispatcherThead ()
dispatch = do (sig, m) <- chRead dch
              if check sig m
              then return ()
              else case Map.lookup sig chs of
                     Nothing -> dispatch chs dch
                     Just ch -> do chWrite ch m
                                   dispatch chs dch
    where
      check sig (Message m) = sig == System && m == "exit"

fork :: DispatcherChannel 
     -> [(Signature, Procedure)]
     -> IO ([ThreadId], [(Signature, RequestChannel)])
fork dch procs = fork' [] [] procs
    where
      fork' :: [ThreadId] 
            -> [(Signature, RequestChannel)] 
            -> [(Signature, Procedure)] 
            -> IO ([ThreadId], [(Signature, RequestChannel)]) 
      fork' ths chs [] = return (ths, chs)
      fork' ths chs ((sig, proc):procs) = do nch <- chNew
                                             tid <- forkIO $ proc nch dch
                                             fork' (tid:ths) ((sig, nch):chs) procs
-}