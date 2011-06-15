{-# LANGUAGE DeriveDataTypeable #-}

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

data Clip = Clip { 
                   timestamp :: UTCTime, --(day and seconds)
     	    	   url       :: String,
                   metadata  :: String
                 } deriving (Eq, Show)
instance Show UTCTime where
    show t = (showGregorian $ utctDay t) ++ ":" ++ (show $ utctDayTime t)

newtype Binder = Binder {
                          clips :: [Clip]
                        } deriving (Show, Typeable)
data Operation = Add | Remove | Search | Look deriving (Show, Eq, Read, Ord) 

emptyBinder :: Binder
emptyBinder = Binder []

