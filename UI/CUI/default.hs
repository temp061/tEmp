{-
  default: the default CUI program
-}

module UI.CUI.Default (cuiProcedure) where

import qualified Data.Map as Map

import Utility.Prim
import Data.Time.Clock

import MetaData.Clip
import MetaData.Types

import UI.CUI.Types

cuiProcedure :: Procedure -- ClientThread ()
cuiProcedure = getCUIMessage >>= handleCUIMessage

getCUIMessage :: ClientThread NormalMessage
getCUIMessage = do cs <- liftMT getContents
	           return $ NM cs

handleCUIMessage :: NormalMessage -> ClientThread ()
handleCUIMessage (NM input) = do result <- dispatch $ lines input
                                 case result of
                                   Left (CUIException e)  -> liftMT $ putStrLn e
                                   Right _                -> liftMT $ putStrLn "ok"

dispatch :: [String] -> ClientThread (Either CUIException Int)
dispatch (c:cs) = post (MetaData, NM c) >> dispatch cs

exit :: [String]
     -> ClientThread (Either CUIException Int)
     -> ClientThread (Either CUIException Int)
exit cs _ = do post $ (UI, NM "exit")
               return $ Right 0

putB :: [String]
         -> ClientThread (Either CUIException Int)
         -> ClientThread (Either CUIException Int)
putB _ dis = do b <- getShared
                liftMT $ putStrLn $ show b
                dis

{-
            where
	      dispatch (c:cs) | c == "EXIT" = "Content is EXIT"
	      	              | otherwise   = c ++ "\n" ++ dispatch cs
-}

--汚いので要検討
{-
dispatch :: [String] -> IO (Either CUIException Int)
dispatch (command:commands) = let (func:args) = words command
                              in if func == "exit"
                                 then Right 0
                                 else 
case dispatch' (words command) of
                                  Left e         -> return $ Left e
                                  Right Nothing  -> return $ Right 0
                                  Right (Just a) -> a >> dispatch commands
           where
             dispatch' :: [String] -> Either CUIException (Maybe (IO ()))
             dispatch' (c:cs) | c == "EXIT" = Right Nothing
                              | otherwise   = case Map.lookup c commandMap of
                                                Just func -> Right $ Just $ func cs
                                                Nothing   -> Left $ CUIException
                                                             "called command is not found.."
-}

{-
commands ==
"cat url\nls -al\nmore file.dat.."
["cat url", "ls -al", "more file.dat",..]
-}