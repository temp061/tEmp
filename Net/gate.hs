module Net.Gate (Gate, push, pop, kick, reject, runGate, Destination) where

import MetaData.Types
import MetaData.Clip
import Network.Socket
import Control.Monad.State
import qualified Data.Map as Map
import System.IO
import Data.Maybe
import Utility.Prim
import Control.Concurrent.STM
import Control.Concurrent

type Destination = String

type Gate = StateT GateState IO

type SendHandle = Handle
type RecvHandle = (Handle, HandlePosn)

data GateState = GateState {
      sendQ     :: TChan Clip,
      recvQ     :: TChan (Clip, Destination),
      links     :: TVar (Map.Map Destination (SendHandle, RecvHandle)),
      blacklist :: TVar [Destination]
    }

-- get :: Gate GateState
push :: Clip -> Gate ()
push c = get >>= \gs -> lift $ atomically $ writeTChan (sendQ gs) c

pop :: Gate (Maybe (Clip, Destination))
pop = get >>= \gs -> let rq = recvQ gs in 
                     lift $ atomically $ 
                          isEmptyTChan rq >>= \emp ->
                              if emp
                              then return Nothing 
                              else readTChan rq >>= return.Just

kick :: Destination -> Gate ()
kick dest = get >>= (\(GateState _ _ ls _) ->  
                     lift (
                           updateTVar ls (Map.delete dest)
                          ))     

reject :: Destination -> Gate ()
reject dest = get >>= (\(GateState _ _ _ bl) -> 
                       lift (
                              updateTVar bl (dest:)
                            )) >> 
              kick dest

updateTVar :: TVar a -> (a -> a) -> IO ()
updateTVar source func = readTVarIO source >>= \target -> atomically $ writeTVar source (func target)

host :: HostName
host = "127.0.0.1"

port :: ServiceName
port = "2011"

runGate :: Gate a -> IO a
runGate act = do (ssock, _) <- socketInit
                 rsock <- socketInit
                 let bl = loadProf
                 tl <- newTVarIO (Map.singleton host (ssock, rsock))
                 tb <- newTVarIO bl
                 sch <- newTChanIO
                 forkIO $ sendProc sch tl
                 rch <- newTChanIO
                 forkIO $ recvProc rch tl tb
                 evalStateT act $ GateState sch rch tl tb

socketInit :: IO (Handle, HandlePosn)
socketInit = withSocketsDo $ do
               (peeraddr:_) <- getAddrInfo Nothing (Just host) (Just port)
               sock <- socket (addrFamily peeraddr) Stream defaultProtocol
               setSocketOption sock KeepAlive 1
               connect sock (addrAddress peeraddr)
               h <- socketToHandle sock ReadWriteMode
               hSetBuffering h (BlockBuffering Nothing) -- 実装依存だけど？
               posn <- hGetPosn h
               return (h, posn)

loadProf :: [Destination]
loadProf = []

sendProc :: TChan Clip -> TVar (Map.Map Destination (SendHandle, RecvHandle)) -> IO ()
sendProc ch tls = atomically (readTChan ch) >>= \c -> 
                  mapClip tls mapM_ (\ls d -> hPutStrLn (fst $ (fromJust $ Map.lookup d ls)) (show c ++ "\x1a"))

recvProc :: TChan (Clip,Destination) -> TVar (Map.Map Destination (SendHandle, RecvHandle)) -> TVar [Destination] -> IO ()
recvProc ch tls tbl = mapClip tls mapM popClip >>= \cds -> mapM_ (atomically.(writeTChan ch)) (catMaybes cds)
    where
      popClip :: Map.Map Destination (SendHandle, RecvHandle) -> Destination -> IO (Maybe (Clip, Destination))
      popClip ls d = do let (_ , (handle, before)) = fromJust $ Map.lookup d ls
                        posn <- hGetPosn handle
                        if before /= posn 
                        then hGetContents handle >>= (\cs -> return $ Just (read cs, d))
                        else return Nothing
{-
mapClip :: TVar (Map.Map Destination (SendHandle, RecvHandle))
        -> ((Destination -> IO a) -> [Destination] -> IO b)
        -> (Map.Map Destination (SendHandle, RecvHandle) -> Destination -> IO a)
        -> IO b
-}
mapClip ls mapper func = readTVarIO ls >>= (\m -> mapper (func m) (Map.keys m))
