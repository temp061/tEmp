module Net.Gate where

import MetaData.Types
import Network.Socket
import Control.Monad.State
import qualified Data.Map as Map
import System.IO
import Data.Maybe
import Control.Applicative

type Destination = String

type Gate = StateT GateState IO

data GateState = GateState {
      links     :: Map.Map Destination Handle,
      blacklist :: [Destination]
    }
-- get :: Gate GateState
push :: Clip -> Gate ()
push c = do (GateState ls bl) <- get
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

init :: Gate ()
init = lift (withSocketsDo $ do
               (peeraddr:_) <- getAddrInfo Nothing (Just host) (Just port)
               sock <- socket (addrFamily peeraddr) Stream defaultProtocol
               setSocketOption sock KeepAlive 1
               connect sock (addrAddress peeraddr)
               h <- socketToHandle sock ReadWriteMode
               hSetBuffering h (BlockBuffering Nothing) -- 実装依存だけど？
               return h
            )
       >>= (\h -> modify (\gs -> gs{links = Map.insert host h (links gs)}))