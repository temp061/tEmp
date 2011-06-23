
chMap :: Map.Map String (TChan String)
chMap = Map.fromList [
         ("output", output),
        ]

opMap :: Map.Map String (TChan String -> IO ())
opMap = Map.fromList [
         ("output", writeStd),
        ]

operation :: Procedure
operation = do map.lookup  opMap
               --  writeTChan output "hello"
               output <- newTChanIO
               input <- newTChanIO
               err <- newTChanIO
               
               ops :: [String]
               ops = "output" : "input" : "err" : []

               chs <- map f ops
               xs = g ops
               do ch <- chs -- ch :: a , chs :: [] a  a <- m a
                  x <- xs   -- x :: b, xs :: [] b
                  return $ forkIO (x ch)
    where 
      f :: String -> TChan String
      g :: String -> TChan String -> IO ()
               

{- stdout.hs
writeStd :: TChan String -> IO ()
writeStd s = forever $ atomically (readTChan s) >>= putStrLn >> hFlush stdout
-}

handle :: NormalMessage -> ClientThread ()
handle m = let (, msg) = translate m