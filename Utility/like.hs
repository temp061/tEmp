{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Utility.Like where

class Like l a | a -> l where
    abst :: (Eq l) => a -> l

like :: (Eq l, Like l a) => a -> a -> Bool
like left right = (abst left) == (abst right)
