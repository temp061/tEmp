module MetaData.Clip(add)  where

import Data.Time.Clock
import Control.Concurrent.STM
import Control.Monad

import Utility.Prim

import UI.CUI.Types

import MetaData.Types

verifyUri :: String -> Bool
verifyUri uri = True

add :: Binder -> Clip -> Binder
add b c = b{clips = c:(clips b)}

{-
add :: Binder -> [String] -> UTCTime -> Either CUIException Binder
add b (uri:md:args) t
    | verifyUri uri == True  = Right b{clips = (Clip t uri md):(clips b)}

add _ _ _ = Left $ CUIException "add: arguments are not enough."
-}
{-
instance Show Binder where
    show b = (readTVar $ clips b) >>= concatMap show'
        where
          show' :: Clip -> String
          show' c = show c ++ "|"

  show b = concat $ map show' (clips b)
      where
        show' :: Clip -> String
        show' c = show c ++ "|"



metaDataManager :: TChan Message -> TChan Message -> IO ()
metaDataManager mdch dch = putStrLn "metadata"

tEmpBinder :: STM Binder
tEmpBinder = do tlist <- newTVar []
                return $ Binder tlist

addHandler :: [String] -> IO () 
addHandler (u:m:_) = let c = (Clip getCurrentTime u m)
                     in do atomically $ addClip c
                           
--(return () , addClip b (Clip getCurrentTime u m))

removeHandler :: [String] -> IO ()
removeHandler (u:m:_) = atomically $ removeClip (Clip getCurrentTime u m)
-}

-- liftMT $ putStrLn (getShared >>= show)
-- (putStr $ show b , b)

{-
 getCurrentTimeがいつ評価されるかについて、吟味の余地あり。
 遅延評価なので、addClipが実行されたときの時間にならない可能性がある。

 *timestampがおそらく、参照される度にその時の現在時刻になってしまうので、
　これをもとに失効を判断するのは危険であると思われる。
-}

{-
addClip :: Clip -> STM ()
addClip c = do
  b <- tEmpBinder
  list <- readTVar $ clips b
  case c `elem` list of 
    True  -> writeTVar (clips b) (map (update c) list)
    False -> writeTVar (clips b) (c : list)
    where
          update target c | c == target = c {timestamp = getCurrentTime}
                          | otherwise   = c
-}

{-
addClip :: Binder -> Clip -> Binder
addClip b c | c `elem` clips b = Binder (map (update c) (clips b)) 
	    | otherwise        = Binder (c : clips b)
	where
	  update target c | c == target = c {timestamp = getCurrentTime}
	  	 	  | otherwise 	= c
-}

{-
removeClip :: Clip -> STM ()
removeClip c = do
  b <- tEmpBinder
  list <- readTVar $ clips b
  writeTVar (clips b) (filter (/= c) list)
-- Binder (filter (/= c) (clips b))

showBinder :: STM String
showBinder = do
  b <- tEmpBinder
  list <- readTVar $ clips b
  return $ concatMap show' list
    where
      show' :: Clip -> String
      show' c = show c ++ "|"
-}
