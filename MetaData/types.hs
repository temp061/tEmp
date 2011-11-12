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

import Text.Parsec hiding (many)
import Control.Applicative

import Utility.Like

data Clip = Clip { 
      timestamp :: UTCTime, --(day and seconds)
      uri       :: String,
      metadata  :: String
    } deriving (Eq, Show, Read)

instance Like (String, String) Clip where
    abst clip = (uri clip, metadata clip)

instance Show UTCTime where
    show t = (showGregorian $ utctDay t) ++ ":" ++ (show $ utctDayTime t)

instance Read UTCTime where
    readsPrec _ input = case parse p "(read UTC)" input of
                          Right (d, t, r) -> [(UTCTime d t, r)]
                          Left err -> error $ show err
        where
          p = (,,) <$> (spaces *> day) <*> (char ':' *> time <* char 's') <*> remain 
          day = fromGregorian <$> (year <* char '-') <*> (month <* char '-') <*> date
          year = read <$> many digit -- Integer
          month = read <$> many digit -- Int
          date = month
          time = picosecondsToDiffTime <$> picoseconds
          picoseconds = read <$> ( (\int frac -> int ++ frac ++ "000000") <$> (many digit <* char '.') <*> many digit )
          remain = many anyChar

newtype Binder = Binder {
                          clips :: [Clip]
                        } deriving (Show, Typeable)

data Operation = Add | Remove | Search | Look | Get | Append deriving (Show, Eq, Read, Ord) 

emptyBinder :: Binder
emptyBinder = Binder []
