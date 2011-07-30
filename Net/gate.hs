module Net.Gate where

import MetaData.Types
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
      links     :: Map.Map Destination Handle,
      blacklist :: [Destination]
    }

data GateMessage = GM {
      order :: String,
      clip  :: Clip,
      dest  :: Destination
}

-- get :: Gate GateState
push :: Clip -> Gate ()
push c = do (GateState _ ls bl) <- get
            mapM_ (putClip ls) $ filter (`notElem` bl) (Map.keys ls)
    where
      putClip ls d = lift $ hPutStrLn (fromJust $ Map.lookup d ls) (show c)

pop :: Gate (Clip, Destination)
pop = undefined

kick :: Destination -> Gate ()
kick = undefined

reject :: Destination -> Gate ()
reject = undefined

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
                   return h
                )
           >>= (\h -> modify (\gs -> gs{links = Map.insert host h (links gs)}))

runGate :: Gate a -> Profile -> TChan GateMessage -> IO a
runGate act prof ch = evalStateT act $ loadProf prof ch

loadProf :: Profile -> TChan GateMessage -> GateState
loadProf prof ch = GateState ch Map.empty []
