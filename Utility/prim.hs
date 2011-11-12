{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Utility.Prim (
                     module Utility.MThread,
                     module Utility.Message,
                     ClientThread,
                     ClientState(..),
                     DispatcherThread,
                     Procedure,
                     Profile(..)
                    ) where

import Utility.MThread
import Utility.Message

import MetaData.Types

import Data.Typeable
import qualified Data.Map as Map

import Control.Concurrent

type ClientThread = MThread () ClientState
data ClientState = UnitState | forall s. (Typeable s) => CS s

type DispatcherThread = MThread () (Map.Map Signature ThreadId)
type Procedure = ClientThread ()

newtype Profile = Profile String
