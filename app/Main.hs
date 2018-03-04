{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           AWSLambda (lambdaMain)
import           AWSViaHaskell (connect)
import           Control.Exception (SomeException, bracket_, catch)
import           Data.Aeson (Value)
import           HLambda.Commands
import           HLambda.Config
import           HLambda.Services
import           HLambda.Types
import           HLambda.Util

main :: IO ()
main =
    bracket_
        (logMessage "hlambda started")
        (logMessage "hlambda stopped")
        mainInner
        `catch` (\e -> logMessage $ "Unhandled exception: " ++ show (e :: SomeException))

mainInner :: IO ()
mainInner = lambdaMain $ \event -> do
    conf <- getAWSConfig

    --stsSession <- connect conf stsService
    --accountId <- getAccountId stsSession
    --putStrLn $ "Account ID: " ++ show accountId

    ssmSession <- connect conf ssmService
    setParameter
        (ParameterName "/path/to/parameter")
        (ParameterString "hello-world")
        ssmSession

    print (event :: Value)
    return ([1, 2, 3] :: [Int])
