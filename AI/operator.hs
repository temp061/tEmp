{-# LANGUAGE TupleSections #-}

module AI.Operator (operation, features) where

import Control.Concurrent
import Control.Monad.Trans
import qualified Data.Map as Map

import Utility.Prim
import Utility.Message

import MetaData.Clip

opMap = Map.fromList [
         ("Add", add), 
         ("ResGet", resget)
        ]

features :: [(String, Signature)] 
features = map (,AI) supports

supports :: [String]
supports = ["Add", "ResGet"]

operation :: Procedure
operation = fetch >>= handle >> operation

handle :: NormalMessage -> ClientThread ()
handle msg = let (op, args) = translate msg
             in case Map.lookup op opMap >>= \handler -> handler args of
                  Just ct -> ct
                  Nothing -> post (UIOut, NM "error AI error")

add _ = Just $ post (MetaData, NM $ "Get " ++ show AI ++ " " ++ "ResGet")

resget cls = let cls' = pruning $ read $ unwords cls
             in Just $ post (Net, NM $ "Release " ++ show cls')
    where
      pruning :: [Clip]-> [Clip]
      pruning cl = cl

translate :: NormalMessage -> (String, [String])
translate (NM msg) = let op:args = words msg
                     in (op, args)