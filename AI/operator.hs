{-# LANGUAGE TupleSections #-}

module AI.Operator (operation, features) where

import Control.Concurrent
import Control.Monad.Trans

import Utility.Prim
import Utility.Message

import MetaData.Types

features :: [(String, Signature)] 
features = map (,AI) supports

supports :: [String]
supports = ["Add"]

operation :: Procedure
operation = fetch >>= \(NM str) -> lift (putStrLn $ "AI: " ++ str) >> operation