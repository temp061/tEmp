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

import Text.Parsec
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
    Just (d, t) -> [(UTCTime d t, "")]
    Nothing -> []
      where
        p :: Parsec (Day, DiffTime)
        p = (,) <$> day <*> (char ':' *> time <* char 's')
        day :: Parsec Day
        day = fromGregorian <$> (year <* char '-') <*> (month <* char '-') <*> date
        -- ap (ap ((liftM fromGregorian) (year <* char '-')) (month <* char '-')) date
        -- ap (ap (fmap fromGregorian (year <* char '-')) (month <* char '-')) date
        -- fmap fromGregorian (year <* char '-') `ap` (month <* char '-') `ap` date
        -- fromGregorian <$> (year <* char '-') <*> (month <* char '-') <*> date
        -- fromGregorian <$> year <* char '-' <*> month <* char '-' <*> date {- これも可能か? -}
        year :: Parsec Integer -- f Integer
        year = read <$> many digit
          -- fmap read (many digit)
        time :: Parsec DiffTime
        time = DiffTime <$>
        
        char :: Char -> Parsec Char
        string :: String -> Parsec String
        many :: Parsec a -> Parsec [a]
        anyChar :: Parsec Char
        
        scheme = liftA2 (++) (try (string "https") <|> string "http") (string "://")
        
        5 + 4
        [5,4,3] + [2,3,4] -> NG
        for(x <- [5,4,3] , y <- [2,3,4])
           [x + y]
        ==> [7,7,7]
        func 5 4
        func [5,4,3] [2,3,4] -> NG
        for(x <- [5,4,3], y <- [2,3,4])
           [func x y]

        (+), func :: Int -> Int -> Int
        become [Int] -> [Int] -> [Int] -> NG
        conv :: (Int -> Int -> Int) -> ([Int] -> [Int] -> [Int])

        5 + 4
        (conv (+)) [5,4,3] [2,3,4] === [7,7,7]
        g = conv (+)
        [5,4,3] `g` [2,3,4] === [7,7,7]
        func 5 4
        (conv func) [5,4,3] [2,3,4]

        conv :: (a -> b -> c) -> ([a] -> [b] -> [c]) -> more convinient
        conv :: (a -> b -> c) -> (cont a -> cont b -> cont c) -> more
        liftM2 or liftA2
 
        (<$>) = fmap
        fmap a b === a <$> b
        lift :: (a -> b) -> m a -> m b
        =:= (a -> b) -> f a -> f b === fmap
        lift x y === x <$> y

        func :: (a -> b -> c) -> f a -> f b -> f c
        === (a -> (b -> c)) -> f a -> f (b -> c)
        ap :: f (a -> b) -> f a -> f b
        ap (lift f x) y === (lift f x) `ap` y
        (<*>) = ap
        ==> (lift f x) <*> y
        === (fmap f x) <*> y
        === f <$> x <*> y

        ~~~ f x y

        liftM :: (a -> b) -> (m a -> m b)
        liftM f a ==> (liftM f) a  === liftM f $ a

        map :: (a -> b) -> [a] -> [b]
        fmap :: (a -> b) -> f a -> f b
        fmap f a ==> (fmap f a) === (fmap f) a === fmap f $ a
        fmap === liftM  (f != m)
        
        f a b [g [c d] h [...] w [...]]
         f -> g c d [h[...] w[...]]
          g -> h [w[...]]
>> === *>
>>= === ?

newtype Binder = Binder {
                          clips :: [Clip]
                        } deriving (Show, Typeable)

data Operation = Add | Remove | Search | Look deriving (Show, Eq, Read, Ord) 

emptyBinder :: Binder
emptyBinder = Binder []
