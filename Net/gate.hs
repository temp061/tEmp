module Net.Gate where

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

data GateState = GateState {
      channel   :: TChan GateMessage,
      links     :: Map.Map Destination (Handle, HandlePosn),
      blacklist :: [Destination]
    }

data GateMessage = GM {
      order :: String,
      clip  :: Clip,
      dest  :: Destination
}

-- get :: Gate GateState
push :: Clip -> Gate ()
push c = (\(GateState _ ls _) -> mapM_ (putClip ls) (Map.keys ls)) =<< get
    where
      putClip ls d = lift $ hPutStrLn (fst $ (fromJust $ Map.lookup d ls)) (show c ++ "\x1a")

pop :: Gate [(Clip, Destination)]
pop = do st <- get
         let ls = links st
         mapM (popClip ls) (Map.keys ls)
    where
      popClip :: Map.Map Destination (Handle, HandlePosn) -> Destination -> Gate (Clip, Destination)
      popClip ls d = lift $ do let (handle, before) = fromJust $ Map.lookup d ls
                               posn <- hGetPosn handle
                               cs <- if before /= posn then hGetContents handle else return ""
                               return (restore cs, d)

kick :: Destination -> Gate ()
kick dest = modify $ \st -> st { links = Map.delete dest (links st) }     

reject :: Destination -> Gate ()
reject dest = modify (\st -> st { blacklist = dest:(blacklist st) }) >> kick dest

host :: HostName
host = "127.0.0.1"

port :: ServiceName
port = "2011"

forkGate :: Gate () -> Profile -> IO (TChan GateMessage)
forkGate proc prof = do ch <- newTChanIO 
                        forkIO $ runGate (gateInit >> proc) prof ch
                        return ch

gateInit :: Gate ()
gateInit = lift (withSocketsDo $ do
                   (peeraddr:_) <- getAddrInfo Nothing (Just host) (Just port)
                   sock <- socket (addrFamily peeraddr) Stream defaultProtocol
                   setSocketOption sock KeepAlive 1
                   connect sock (addrAddress peeraddr)
                   h <- socketToHandle sock ReadWriteMode
                   hSetBuffering h (BlockBuffering Nothing) -- 実装依存だけど？
                   posn <- hGetPosn h
                   return (h, posn)
                )
           >>= (\(h, posn) -> modify (\gs -> gs{links = Map.insert host (h, posn) (links gs)}))

runGate :: Gate a -> Profile -> TChan GateMessage -> IO a
runGate act prof ch = evalStateT act $ loadProf prof ch

loadProf :: Profile -> TChan GateMessage -> GateState
loadProf prof ch = GateState ch Map.empty []
