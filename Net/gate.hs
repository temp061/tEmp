module Net.Gate (Gate, push, pop, kick, reject, initialState, runGate, Destination, GateState) where

import MetaData.Types
import MetaData.Clip
import Network.Socket
import Control.Monad.State
import System.IO
import Data.List
import Utility.Prim
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

import Numeric
import Data.Char
import Text.Parsec hiding (many)
import Control.Applicative


type Destination = String

namingDest :: SockAddr -> Destination
namingDest (SockAddrInet (PortNum port) ip) = unwords [show ip, show port]

resolveAddr :: Destination -> SockAddr
resolveAddr dest = let [ip, port] = words dest
                   in SockAddrInet (PortNum $ read port) (read ip)

type Gate = StateT GateState IO

type SendHandle = Handle
type RecvHandle = (Handle, HandlePosn)

data GateState = GateState {
      sendQ         :: TChan Clip,
      postId        :: ThreadId,
      receivedPool  :: TVar [(Clip, Destination)],
      links         :: TVar [Destination],
      blacklist     :: TVar [Destination]
    }

push :: Clip -> Gate ()
push c = get >>= \gs -> lift $ atomically $ writeTChan (sendQ gs) c

pop :: Gate [(Clip, Destination)]
pop = get >>= \gs -> lift $ readTVarIO $ receivedPool gs
-- pop = get >>= \gs -> lift $ atomically $ readTVar (receivedPool gs) >>= \pool -> if pool == [] then retry else return pool 

kick :: Destination -> Gate ()
kick   dest = adjust links delete dest

reject :: Destination -> Gate()
reject dest = adjust blacklist (:) dest >> kick dest

adjust ::  (GateState -> TVar [Destination]) -> (Destination -> [Destination] -> [Destination]) -> Destination -> Gate ()
adjust target instruction dest = get >>= \gs -> lift $ updateTVarIO (target gs) (instruction dest)

updateTVarIO :: TVar a -> (a -> a) -> IO ()
updateTVarIO source func = readTVarIO source >>= \target -> atomically $ writeTVar source (func target)

host :: Destination
host = namingDest $ SockAddrInet (PortNum $ 0xdc07) 0x0100007F -- 0x0100007F === 127.0.0.1

own :: HostName
own = "127.0.0.1"

servicePort :: ServiceName
servicePort = "2011" -- ポート2011の意味

{-
connectPort = ServiceName
connectPort = "0xb60f" -- 4022
-}

initialState :: IO GateState
initialState = do let bl = loadProf
                  tls <- newTVarIO [host]
                  tbl <- newTVarIO bl
                  sch <- newTChanIO
                  pool <- newTVarIO []
                  ptid <- forkIO $ postProc pool tls tbl
                  forkIO $ sendProc sch tls
                  return $ GateState sch ptid pool tls tbl 

runGate :: Gate a -> GateState -> IO (a, GateState)
runGate act gs = runStateT act gs

postProc :: TVar [(Clip, Destination)] -> TVar [Destination] -> TVar [Destination] -> IO ()
postProc pool tls tbl = withSocketsDo $ do
                          (postinfo:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) (Just own) (Just servicePort)
                          sock <- socket (addrFamily postinfo) Stream defaultProtocol
                          putStrLn $ show $ addrAddress postinfo
                          bindSocket sock $ addrAddress postinfo
                          listen sock 5 -- 5は接続待ちキューの長さ。最大はシステム依存(通常5)
                          standby sock
    where
      standby :: Socket -> IO ()
      standby parent = accept parent >>= \(sock, addr) -> 
                       readTVarIO tbl >>= \bl -> let dest = namingDest addr
                                                 in if dest `notElem` bl
                                                    then entry dest >> forkIO (receiver sock pool dest tls) >> return ()
                                                    else return () -- else case : "a menber of the black list(bl)" 
                       >> standby parent
      entry :: Destination -> IO ()
      entry dest = readTVarIO tls >>= \ls -> 
                   atomically.writeTVar tls $ if dest `elem` ls then dest:ls else ls

receiver :: Socket -> TVar [(Clip, Destination)] -> Destination -> TVar [Destination] -> IO () 
receiver sock pool dest tls = do hdl <- socketToHandle sock ReadMode
                                 hSetBuffering hdl LineBuffering
                                 msg <- hGetContents hdl
                                 receiver' $ lines msg
    where
      receiver' [] = return ()
      receiver' (r:rs) = readTVarIO tls >>= \ls -> if dest `elem` ls
                                                   then (atomically $ readTVar pool >>= \p -> writeTVar pool (((read r), dest):p)) >> receiver' rs 
                                                   else return ()
                                  
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
                    putStrLn $ show addr
                    connect sock addr
                    h <- socketToHandle sock WriteMode
                    hSetBuffering h LineBuffering
                    return h

-- linkToHandle:[複数to複数]対応なのでちょっと気に入らない
sendProc :: TChan Clip -> TVar [Destination] -> IO ()
sendProc ch tls = forever $ atomically (readTChan ch) >>= \clip -> 
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