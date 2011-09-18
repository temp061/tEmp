module Net.Gate (push, pop, kick, reject, forkGate) where

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
push c = get >>= \gs -> lift $ writeTChanIO (sendQ gs) c

pop :: Gate [(Clip, Destination)]
pop = mapClip mapM popClip
    where
      popClip :: Map.Map Destination (SendHandle, RecvHandle) -> Destination -> IO (Clip, Destination)
      popClip ls d = do let (_ , (handle, before)) = fromJust $ Map.lookup d ls
                        posn <- hGetPosn handle
                        cs <- if before /= posn then hGetContents handle else return ""
                        return (read cs, d)

mapClip cmap func = get >>= (\(GateState _ _ ls _) -> lift ( readTVarIO ls >>= (\m -> 
                                                                           cmap (func m) (Map.keys m))
                                                      ))

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

forkGate :: Profile -> Gate ()
forkGate prof = lift (do (ssock, _) <- socketInit
                         rsock <- socketInit
                         let bl = loadProf prof
                         tl <- newTVarIO (Map.singleton host (ssock, rsock))
                         tb <- newTVarIO bl
                         sch <- newTChanIO
                         forkIO $ sendProc sch tl
                         rch <- newTChanIO
                         forkIO $ recvProc rch tl tb
                         return $ GateState sch rch tl tb
                     )
                >>= put 

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
{-
runGate :: Gate a -> Profile -> TChan GateMessage -> IO a
runGate act prof ch = evalStateT act $ loadProf prof ch
-}
loadProf :: Profile -> [Destination]
loadProf prof = []

sendProc :: TChan Clip -> TVar (Map.Map Destination (SendHandle, RecvHandle)) -> IO ()
sendProc ch tls = readTChan ch >>= \c -> mapClip tls mapM_ (\ld -> hPutStrLn (fst $ (fromJust $ Map.lookup d ls)) (show c ++ "\x1a"))

recvProc :: TChan Clip -> TVar (Map.Map Destination (SendHandle, RecvHandle)) -> TVar [Destination] -> IO ()
recvProc ch tls tbl = undefined

mapClip ls cmap func = readTVarIO ls >>= (\m -> cmap (func m) (Map.keys m))

