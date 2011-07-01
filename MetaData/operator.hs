{-# LANGUAGE ScopedTypeVariables #-}

module MetaData.Operator (operation) where

import Data.Typeable
import qualified Data.Map as Map

import Utility.Prim
import Data.Time.Clock

import MetaData.Clip
import MetaData.Types

opMap = Map.fromList [ (Add, handleOn add),
                       -- (Remove, handleOn remove), 
                       (Look, look)
                       -- (Search, handleOn search)
                     ]

operation :: Procedure
operation = {- liftMT (putStrLn "MetaData.Operator.") >> -} fetch >>= handle

handle :: NormalMessage -> ClientThread ()
handle m  = let (op, args) = translate m
            in case Map.lookup op opMap >>= \handler -> handler args of
                 Just ct -> ct --operation --handler -> handler args
                 Nothing -> post (UIOut, NM "error Operation error")
               >> operation

handleOn :: (Binder -> Clip -> Binder) -> [String] -> Maybe (ClientThread ())
handleOn func [] = Nothing
handleOn func args = Just $ do (CS s) <- getStatus
                               t <- liftMT getCurrentTime
                               let dl = tokenize args
                                   cl = loop t dl
                                   Just b = cast s
                               setStatus $ CS $ head $ map (func b) cl
    where 
      loop :: UTCTime -> [(String, String)] -> [Clip]
      loop _ [] = []
      loop t ((uri, md):xs) = (Clip t uri md) : (loop t xs)

look :: [String] -> Maybe (ClientThread ())
look _ = Just $ do (CS s) <- getStatus
                   let Just b :: Maybe Binder = cast s
                   post (UIOut, NM $ "output " ++ show b)
                   return ()

tokenize :: [String] -> [(String, String)]
tokenize [] = []
tokenize (x:xs) = parse x ++ tokenize xs
    where 
      parse x = [("", "")]
 -- ["uri1|md1,md2","uri2,uri3|md3,md4"]

translate :: NormalMessage -> (Operation, [String])
translate (NM msg) = let op:args = words msg
                     in (read op, args)

