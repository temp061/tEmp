module UI.Operator (operation) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Utility.Prim
import System.IO

opList :: [(String, (TChan String -> IO()))]
opList = [ ("output", writeStd), ("error", writeErr), ("input", inputStd) ] -- ,etc ...

msgPrptyList :: [(__KEY__, __DEST__)]
msgPrptyList = [  ]

operation :: Procedure
operation = {- liftMT (putStrLn "UI.Operator.") >> -} liftMT ( mapM fork opList ) >>= handle
  where
    fork :: (String, (TChan String -> IO ())) -> IO (String, (TChan String))
    fork (op,procedure) = do ch <- newTChanIO
                             forkIO $ procedure ch
                             return (op, ch)

writeHandle :: Handle -> TChan String -> IO ()
writeHandle h s = forever $ atomically (readTChan s) >>= hPutStrLn h >> hFlush h

writeStd :: TChan String -> IO ()
writeStd = writeHandle stdout

readHandle :: TChan String -> IO()
readHandle ch = do s <- getContents
                   atomically $ writeTChan ch s

writeErr :: TChan String -> IO ()
writeErr = writeHandle stderr

handle :: [(String, TChan String)] -> ClientThread ()
handle chList = do
  normal <- fetch
  let (op, msg) = translate normal
  case lookup op chList of
    Just ch -> liftMT $ atomically $ writeTChan ch msg
    Nothing -> printErr chList "error (operation not found)"
  handle chList
  
printErr :: [(String, TChan String)] -> String -> ClientThread ()
printErr chList msg = let Just ch_err = lookup "error" chList
                      in  liftMT $ atomically $ writeTChan ch_err msg

translate :: NormalMessage -> (String, String)
translate (NM msg) = let op:args = words msg
                     in (op, unwords args)
