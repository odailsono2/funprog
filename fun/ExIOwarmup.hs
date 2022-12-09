module IOwarmup where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    )

-- what is the type?  (don't cheat)
whatIsMyType =
    do v1 <- getChar
       v2 <- getChar
       putStrLn $ "\nI have all I need:" ++ [v1,v2]
       return [v1,v2]

putStr :: String -> IO ()
putStr = undefined

putStrLn :: String -> IO ()
putStrLn = undefined

getLine :: IO String
getLine = undefined

putNtimes :: Integral i => i -> Char -> IO ()
putNtimes = undefined

doNtimes :: Integral i => i -> IO a -> IO [a]
doNtimes = undefined

doForever :: IO a -> IO ()
doForever = undefined

when :: Bool -> IO () -> IO ()
when = undefined

-- consult read.txt to learn about read
getInteger :: IO Integer
getInteger = undefined

