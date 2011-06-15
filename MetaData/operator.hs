module MetaData.Operator (operation) where

import Data.Typeable
import qualified Data.Map as Map

import Utility.Prim
import Data.Time.Clock

import MetaData.Clip
import MetaData.Types

opMap = Map.fromList [ (Add, handleOn add) 
                       -- (Remove, remove), 
                       -- (Look, toString),
                       -- (Search, search)
                     ]

operation :: Procedure
operation = fetch >>= handle

handle :: NormalMessage -> ClientThread ()
handle m  = let (op, args) = translate m
            in case Map.lookup op opMap of
                 Just handler -> handler args
                 Nothing -> post (UI, NM "Operation error")
               >> operation

handleOn :: (Binder -> Clip -> Binder) -> [String] -> ClientThread ()
handleOn func args = do (CS s) <- getStatus
                        t <- liftMT getCurrentTime
                        let dl = tokenize args
                            cl = loop t dl
                            Just b = cast s
                        return $ map (func b) cl
                        return ()
    where 
      loop :: UTCTime -> [(String, String)] -> [Clip]
      loop _ [] = []
      loop t ((uri, md):xs) = (Clip t uri md) : (loop t xs)

tokenize :: [String] -> [(String, String)]
tokenize [] = []
tokenize (x:xs) = parse x ++ tokenize xs
    where 
      parse x = [("", "")]
 -- ["uri1|md1,md2","uri2,uri3|md3,md4"]

translate :: NormalMessage -> (Operation, [String])
translate (NM msg) = let op:args = words msg
                     in (read op, args)
