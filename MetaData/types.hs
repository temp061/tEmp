{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module MetaData.Types (
                       Clip(..),
                       Binder,
                       Operation(..),
                       clips,
                       emptyBinder
                      ) where

import Data.Typeable
import Data.Time.Clock
import Data.Time.Calendar

import Utility.Like

data Clip = Clip { 
      timestamp :: UTCTime, --(day and seconds)
      uri       :: String,
      metadata  :: String
    } deriving (Eq, Show)

instance Like (String, String) Clip where
    abst clip = (uri clip, metadata clip)

instance Show UTCTime where
    show t = (showGregorian $ utctDay t) ++ ":" ++ (show $ utctDayTime t)

newtype Binder = Binder {
                          clips :: [Clip]
                        } deriving (Show, Typeable)

data Operation = Add | Remove | Search | Look deriving (Show, Eq, Read, Ord) 

emptyBinder :: Binder
emptyBinder = Binder []
