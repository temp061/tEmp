{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MetaData.Clip ( Clip(..) )  where

import Network.URI

import Text.Parsec hiding (many, option, (<|>))

import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

import Utility.Like

data Clip = Clip { 
      timestamp :: UTCTime, --(day and seconds)
      uri       :: URI,
      metadata  :: String
    } deriving (Eq, Show, Read)
{-
newClip :: UTCTime -> URI -> String -> Clip
newClip time uri meta = isURI uri 
-}
instance Like (String, String) Clip where
    abst clip = (show (uri clip), metadata clip)

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

instance Read URI where
    readsPrec _ input = case parse uriParser "(read URI)" input of
                          Right result -> [result]
                          Left err -> error $ show err
        where
          uriParser = (,) <$> (spaces *> uri) <*> ((char ',') *> remain)
          uri = fromJust.parseURI <$> many1 (noneOf ",")
          remain = many anyChar
