{-# LANGUAGE DeriveDataTypeable #-}

module MetaData.Binder (
                       Binder,
                       clips,
                       add,
                       remove,
                       empty
                      ) where

import Data.Typeable

import MetaData.Clip
import Utility.Like

newtype Binder = Binder {
                          clips :: [Clip]
                        } deriving (Show, Typeable)

empty :: Binder
empty = Binder []

add :: Binder -> Clip -> Binder
add b c = b{clips = c:(clips b)}

remove :: Binder -> Clip -> Binder
remove b c = b{clips = filter without (clips b)}
             where
               without :: Clip -> Bool
               without clip = not $ c `like` clip

