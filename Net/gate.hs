module Net.Gate (Gate, push, pop, kick, reject, initialState, runGate, Destination, GateState) where

import MetaData.Types
import MetaData.Clip
import Network.Socket
import Control.Monad.State
import System.IO
import Data.Maybe
import Utility.Prim
import Control.Concurrent.STM
import Control.Concurrent

type Destination = String

namingDest :: SockAddr -> Destination
namingDest addr = show addr

resolveAddr :: Destination -> SockAddr
resolveAddr dest = read dest

type Gate = StateT GateState IO

type SendHandle = Handle
type RecvHandle = (Handle, HandlePosn)

data GateState = GateState {
      sendQ     :: TChan Clip,
      postId    :: ThreadId
      pool      :: TVar [(Clip, Destination)],
      links     :: TVar [Destination],
      blacklist :: TVar [Destination]
    }

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

host :: Destination
host = namingDest $ SockAddrInet (read servicePort) 0x7F000001 -- 0x7F000001 === 127.0.0.1

servicePort :: ServiceName
servicePort = "2011"

{-
connectPort = ServiceName
connectPort = "4022"
-}

initialState :: IO GateState
initialState = do let bl = loadProf
                  tls <- newTVarIO [host]
                  tbl <- newTVarIO bl
                  sch <- newTChanIO
                  pool <- newTVarIO
                  ptid <- forkIO $ postProc pool tls tbl
                  forkIO $ sendProc sch tls
                  return $ GateState sch ptid pool tls tbl 

runGate :: Gate a -> GateState -> IO (a, GateState)
runGate act gs = runStateT act gs

postProc :: TVar [(Clip, Destination)] -> TVar [Destination] -> TVar [Destination] -> IO ()
postProc pool tls tbl = withSocketsDo $ do
                          (postinfo:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just servicePort)
                          sock <- socket (addrFamily postinfo) Stream defaultProtocol
                          bindSocket sock $ addrAddress postinfo
                          listen sock 5 -- 5は接続待ちキューの長さ。最大はシステム依存(通常5)
                          standby
    where
      standby :: IO ()
      standby = accept sock >>= \(sock, addr) -> 
                let dest = namingDest addr
                in readTVarIO tbl >>= \bl -> if dest `notElem` bl
                                             then entry dest >> forkIO (receiver sock pool dest tls)
                                             else return () -- else case : "a menber of the black list(bl)" 
                >> standby
      entry :: Destination -> IO ()
      entry dest = readTVarIO tls >>= \ls -> 
                   atomically.writeTVarIO $ if dest `elem` ls then dest:ls else ls

receiver :: Socket -> TVar [(Clip, Destination)] -> Destination -> TVar [Destination] -> IO () 
receiver sock pool dest tls = do hdl <- socketToHandle sock ReadMode
                                 hSetBuffering hdl LineBuffering
                                 msg <- hGetContents hdl
                                 receiver' $ lines msg
    where
      receiver' [] = return ()
      receiver' (r:rs) = readTVarIO tls >>= \ls -> if dest `elem` ls then atomically.writeTVar pool (read r) >> receiver' rs else return ()
                                  
loadProf :: [Destination]
loadProf = []

searchLink :: TVar [Destination] -> IO ()
searchLink tls = return ()

sendSocket :: Destination -> IO Handle
sendSocket dest = withSocketsDo $ do 
                    let addr = resolveAddr dest
                    (hostname, portno) <- getNameInfo [] True True addr
                    (addrinfo:_) <- getAddrInfo Nothing hostname portno
                    sock <- socket (addrFamily addrinfo) Stream defaultProtocol
                    setSocketOption sock KeepAlive 1
                    connect sock addr
                    h <- socketToHandle sock WriteMode
                    hSetBuffering h LineBuffering
                    return h

sendProc :: TChan Clip -> TVar [Destination] -> IO ()
sendProc ch tls = atomically (readTChan ch) >>= \clip -> 
                  searchLink tls >> linkToHandle tls >>= mapM_ (flip hPutStrLn $ show clip)
    where
      linkToHandle tls = readTVarIO tls >>= mapM sendSocket 

{-
recvProc :: TChan (Clip,Destination) -> TVar (Map.Map Destination (SendHandle, RecvHandle)) -> TVar [Destination] -> IO ()
recvProc ch tls tbl = readTVarIO tbl >>= \bl -> mapClip tls mapM (popClip bl) >>= \cds -> mapM_ (atomically.(writeTChan ch)) (catMaybes cds)
    where
      popClip :: [Destination] -> Map.Map Destination (SendHandle, RecvHandle) -> Destination -> IO (Maybe (Clip, Destination))
      popClip bl ls d 
          | d `elem` bl = return Nothing
          | otherwise   = do let (_ , (handle, before)) = fromJust $ Map.lookup d ls
                             posn <- hGetPosn handle
                             if before /= posn 
                             then hGetContents handle >>= (\cs -> return $ Just (read cs, d))
                             else return Nothing

mapClip :: TVar (Map.Map Destination (SendHandle, RecvHandle))
        -> ((Destination -> IO a) -> [Destination] -> IO b)
        -> (Map.Map Destination (SendHandle, RecvHandle) -> Destination -> IO a)
        -> IO b
mapClip ls mapper func = readTVarIO ls >>= (\m -> mapper (func m) (Map.keys m))
-}