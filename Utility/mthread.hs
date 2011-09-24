{-
  分割され、スレッド上で並行実行されるモジュールを簡易的に扱う為のモナド(試作)
-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utility.MThread (ThreadMessage(..), 
                      GenMThread, MThread,
                      forkMT, runMT, liftMT,
                      getShared, setShared, modifyShared,
                      getStatus, setStatus, modifyStatus,
                      fetch, tryFetch, post, require, 
                      killMThread
                     ) where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

class ThreadMessage msg tm where
    code   :: tm  -> msg
    decode :: msg -> tm

instance (ThreadMessage msg tm) => ThreadMessage (Maybe msg) (Maybe tm) where
    code (Just tm) = Just $ code tm
    code Nothing = Nothing
    decode (Just msg) = Just $ decode msg
    decode Nothing = Nothing

data ThreadState msg sh st = 
    TS {
      channel :: TChan msg,
      owner   :: TChan msg,
      threads :: TVar (Map.Map ThreadId (TChan msg)),
      shared  :: TVar sh,
      status  :: st
    }

type GenMThread msg sh st = StateT (ThreadState msg sh st) IO
type MThread sh st = GenMThread String sh st

forkMT :: st                                          -- initial local state
       -> GenMThread msg sh st ()                     -- thread action
       -> GenMThread msg sh ost ThreadId              -- thread ID
forkMT nst proc = 
    do oth <- get
       nch <- lift newTChanIO
       tid <- lift $ forkIO $ evalStateT proc $ 
              TS nch (channel oth) (threads oth) (shared oth) nst
       lift $ atomically $ do let tv = threads oth
                              om <- readTVar tv 
                              writeTVar tv $ Map.insert tid nch om
       return tid
                    
runMT :: GenMThread msg sh st a               -- exec action
      -> sh                                   -- initial shared state
      -> st                                   -- initial local state
      -> IO a                                 -- result
runMT mth ish ist = do nch <- newTChanIO
                       noch <- newTChanIO
                       nsh <- newTVarIO ish
                       nchs <- newTVarIO Map.empty
                       evalStateT mth $ TS nch noch nchs nsh ist

liftMT :: IO a -> GenMThread msg sh st a
liftMT act = lift act

accessSTM :: (ThreadState msg sh st -> STM a)
          -> GenMThread msg sh st a
accessSTM f = do ts <- get
                 lift $ atomically $ f ts

getShared :: GenMThread msg sh st sh
getShared = accessSTM $ readTVar.shared

setShared :: sh -> GenMThread msg sh st ()
setShared nsh = accessSTM $ (flip writeTVar nsh).shared

modifyShared :: (TVar sh -> STM (TVar sh)) -> GenMThread msg sh st ()
modifyShared f = do ts <- get
                    nsh <- lift $ atomically $ f $ shared ts
                    put $ ts {shared = nsh}

getStatus :: GenMThread msg sh st st
getStatus = get >>= return.status

setStatus :: st -> GenMThread msg sh st ()
setStatus nst = do ts <- get
                   put $ ts {status = nst}

modifyStatus :: (st -> st) -> GenMThread msg sh st ()
modifyStatus f = modify $ \ts -> ts {status = f $ status ts}

fetch :: (ThreadMessage msg tm) => GenMThread msg sh st tm
fetch = (accessSTM $ readTChan.channel) >>= return.decode

tryFetch :: (ThreadMessage msg tm) => GenMThread msg sh st (Maybe tm)
tryFetch = (accessSTM $ extract.channel) >>= return.decode
    where
      extract ch = isEmptyTChan ch >>= \emp -> 
                   if emp 
                   then return Nothing 
                   else readTChan ch >>= return.Just

post :: (Show msg, ThreadMessage msg om) => om -> GenMThread msg sh st () 
post om = let m = code om
          in accessSTM $ (flip writeTChan m).owner

require :: (ThreadMessage msg cm) => ThreadId -> cm -> GenMThread msg sh st ()
require tid msg = accessSTM $ \ts -> do chs <- readTVar $ threads ts
                                        case Map.lookup tid chs of
                                          Nothing -> return ()
                                          Just ch -> writeTChan ch $ code msg
                                                    
killMThread :: ThreadId -> GenMThread msg sh st ()
killMThread tid = do accessSTM $ \ts -> do let tv = threads ts
                                           chs <- readTVar tv
                                           writeTVar tv $ Map.delete tid chs
                     lift $ killThread tid