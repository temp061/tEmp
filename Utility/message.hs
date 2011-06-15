{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances,
             IncoherentInstances
  #-}

module Utility.Message where

import Text.Parsec hiding (many)
import Control.Applicative 

import Utility.MThread
import MetaData.Types

data Signature = UI | AI | Net | MetaData | System deriving (Eq, Ord, Show, Read)

newtype NormalMessage = NM String

instance Show NormalMessage where
    show (NM cs) = cs

instance ThreadMessage String NormalMessage where
    code = show
    decode = NM

instance ThreadMessage String (Signature, NormalMessage) where
    code (sig, m) = show sig ++ "|" ++ show m
    decode cs = let (sig, m) = parseM
                in (read sig, NM m)
        where
          parseM = case parse p "(decode message)" cs of
                     Left err -> error $ show err
                     Right (sig, m) -> (sig, m)
          p = liftA2 (,) (many $ noneOf "|") ((char '|') *> (many $ anyChar))

