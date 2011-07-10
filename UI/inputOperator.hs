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
      p (_,ch) = do cl <- liftMT $ atomically $ readTChan ch
                    let w:ws = words cl
                    mapM (post' cl) (filter (\(key, _) -> key == w) destList)
                    handle chList
                        where
                            post' :: String -> (String, Signature) -> ClientThread ()
                            post' cl (_,sig) = post (sig, NM cl)

-- ************* --

inputStd :: TChan String -> IO()
inputStd ch = do cs <- getContents
                 mapM_ (\s -> atomically $ writeTChan ch s) (lines cs)

