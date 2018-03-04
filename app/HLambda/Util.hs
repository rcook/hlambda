module HLambda.Util
    ( logMessage
    ) where

import           System.IO (hFlush, stdout)

logMessage :: String -> IO ()
logMessage s = putStrLn s >> hFlush stdout
