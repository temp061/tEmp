module MetaData.Clip(add, remove)  where

import Data.Time.Clock
import Control.Concurrent.STM
import Control.Monad

import Utility.Prim
import Utility.Like

import UI.CUI.Types

import MetaData.Types

verifyUri :: String -> Bool
verifyUri uri = True

add :: Binder -> Clip -> Binder
add b c = b{clips = c:(clips b)}

remove :: Binder -> Clip -> Binder
remove b c = b{clips = filter without (clips b)}
             where
               without :: Clip -> Bool
               without clip = not $ c `like` clip