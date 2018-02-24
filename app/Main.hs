module Main (main) where

import           AWSLambda
import           Data.Aeson (Value)

main :: IO ()
main = lambdaMain $ \event -> do
    putStrLn "This goes to the log"
    print (event :: Value)
    return ([1, 2, 3] :: [Int])
