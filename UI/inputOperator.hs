module UI.InputOperator (operation) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Utility.Prim
import System.IO

import UI.Operator

opList :: [(String, (TChan String -> IO()))]
opList = [ ("input", inputStd) ] -- ,etc ...

destList :: [(String, Signature)] 
destList = [ ("Add", MetaData), ("Look", MetaData) ]

operation :: Procedure
operation = genOperation handle opList

handle :: [(String, TChan String)] -> ClientThread ()
handle chList = mapM_ p chList 
    where 
      p (msg,ch) = do msg <- liftMT $ atomically $ readTChan ch
                      mapM post' (filter (\(key, _) -> key == msg) destList)
                      handle chList
                          where
                            post' :: (String, Signature) -> ClientThread ()
                            post' (msg,sig) = post (sig, NM msg)

-- ************* --

inputStd :: TChan String -> IO()
inputStd ch = do cs <- getContents
                 mapM_ (\s -> atomically $ writeTChan ch s) (lines cs)

