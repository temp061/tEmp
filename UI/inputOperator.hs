module UI.InputOperator (operation, features) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Utility.Prim
import System.IO

import UI.Operator

import qualified AI.Operator as AI
import qualified Net.Operator as Net
-- import qualified UI.InputOperator as UIIn
import qualified UI.OutputOperator as UIOut
import qualified MetaData.Operator as MetaData

opList :: [(String, (TChan String -> IO()))]
opList = [ ("input", inputStd) ] -- ,etc ...

features :: [(String, Signature)] 
features = []

allFeature = features ++ AI.features ++ Net.features ++ UIOut.features ++ MetaData.features

operation :: Procedure
operation = genOperation handle opList

handle :: [(String, TChan String)] -> ClientThread ()
handle chList = mapM_ p chList 
    where 
      p (_,ch) = do cl <- liftMT $ atomically $ readTChan ch
                    let w:ws = words cl
                    mapM (post' cl) (filter (\(key, _) -> key == w) allFeature)
                    handle chList
                        where
                            post' :: String -> (String, Signature) -> ClientThread ()
                            post' cl (_,sig) = post (sig, NM cl)

-- ************* --

inputStd :: TChan String -> IO()
inputStd ch = do cs <- getContents
                 mapM_ (\s -> atomically $ writeTChan ch s) (lines cs)

