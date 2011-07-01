module UI.Operator (genOperation, translate) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Utility.Prim
import System.IO

genOperation :: ([(String, TChan String)] -> ClientThread ()) -> [(String, (TChan String -> IO()))] -> Procedure
genOperation handle opList = liftMT ( mapM fork opList ) >>= handle
    where
      fork :: (String, (TChan String -> IO ())) -> IO (String, (TChan String))
      fork (op,procedure) = do ch <- newTChanIO
                               forkIO $ procedure ch
                               return (op, ch)

translate :: NormalMessage -> (String, String)
translate (NM msg) = let op:args = words msg
                     in (op, unwords args)
