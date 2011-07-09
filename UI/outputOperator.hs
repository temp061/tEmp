module UI.OutputOperator (operation) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Utility.Prim
import System.IO

import UI.Operator

opList :: [(String, (TChan String -> IO()))]
opList = [ ("output", writeStd), ("error", writeErr) ] -- ,etc ...

operation :: Procedure
operation = genOperation handle opList

handle :: [(String, TChan String)] -> ClientThread ()
handle chList = do
  normal <- fetch
  let (op, msg) = translate normal
  case lookup op chList of
    Just ch -> liftMT $ atomically $ writeTChan ch msg
    Nothing -> printErr chList "error (operation not found)"
  handle chList

-- ************* --

writeHandle :: Handle -> TChan String -> IO ()
writeHandle h s = forever $ atomically (readTChan s) >>= hPutStrLn h >> hFlush h

writeStd :: TChan String -> IO ()
writeStd = writeHandle stdout

writeErr :: TChan String -> IO ()
writeErr = writeHandle stderr
 
printErr :: [(String, TChan String)] -> String -> ClientThread ()
printErr chList msg = let Just ch_err = lookup "error" chList
                      in  liftMT $ atomically $ writeTChan ch_err msg
