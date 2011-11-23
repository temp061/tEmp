{-# LANGUAGE ScopedTypeVariables, TupleSections, ViewPatterns #-}

module MetaData.Operator (operation, features) where

import Network.URI

import Text.Parsec hiding (many, option, (<|>))

import qualified Data.Map as Map
import Data.Time.Clock
import Data.Typeable
import Data.List
import Data.Maybe

import Control.Monad.Trans
import Control.Applicative

import Utility.Prim

import MetaData.Clip
import MetaData.Types
import MetaData.Binder

import Net.Gate (Destination) 

opMap = Map.fromList [ (Add, handleOn add),
                       (Remove, handleOn remove), 
                       (Look, look),
                       (Get, get),
                       (Append, append)
                       -- (Search, handleOn search)
                     ]

features :: [(String, Signature)]
features = map (,MetaData) supports

supports :: [String]
supports = ["Add", "Look", "Remove", "Get", "Append"]

operation :: Procedure
operation = fetch >>= handle

handle :: NormalMessage -> ClientThread ()
handle m  = let (op, args) = translate m
            in case Map.lookup op opMap >>= \handler -> handler args of
                 Just ct -> ct --operation --handler -> handler args
                 Nothing -> post (UIOut, NM "error MetaData.Operation error")
               >> operation

handleOn :: (Binder -> Clip -> Binder) -> String -> Maybe (ClientThread ())
handleOn op [] = Nothing
handleOn op args = Just $ do (CS s) <- getStatus
                             t <- lift getCurrentTime
                             let clips = map (cliping t) (tokenize $ words args)
                                 Just b = cast s
                             setStatus $ CS $ foldl' op b clips
    where
      cliping :: UTCTime -> (String, String) -> Clip
      cliping t (uri, md) = Clip t (fromJust $ parseURI uri) md

get :: String -> Maybe (ClientThread ())
get (words -> (dest:code:_)) = Just $ do (CS s) <- getStatus
                                         let Just b :: Maybe Binder = cast s
                                         post ((read dest::Signature), NM $ code ++ " " ++ show (clips b))

look :: String -> Maybe (ClientThread ())
look _ = get "UIOut output"

append :: String -> Maybe (ClientThread ())
append (read -> recv :: [(Clip, Destination)]) = Just $ do (CS s) <- getStatus
                                                           let Just b :: Maybe Binder = cast s
                                                           (setStatus $ CS (b {clips = (clips b) ++ (map fst recv)}))

tokenize :: [String] -> [(String, String)]
tokenize [] = []
tokenize (x:xs) = let (us, ms) = parse' x
                  in (concatMap (\u -> map (u,) ms) us) ++ tokenize xs
    where
      -- ["uri1|md1,md2","uri2,uri3|md3,md4"]
      parse' :: String ->([String], [String])
      parse' cs = case parse tokenizer "(input toknizer)" cs of
                    Left err -> error $ show err
                    Right r -> r
      tokenizer = (,) <$> uris <*> ((char '|') *> mds)
      uris = (uri `sepBy1` (char ','))
      mds =  (md `sepBy1` (char ','))
      uri = many1 $ noneOf ",|"
      md  = many1 $ noneOf ","

translate :: NormalMessage -> (Operation, String)
translate (NM msg) = let op:args = words msg
                     in (read op, unwords args)
