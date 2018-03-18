{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           AWSLambda (lambdaMain)
import           Control.Exception (SomeException, bracket_, catch)
import           Data.Aeson (Value)
import           HLambda.Commands
import           HLambda.Config
import           HLambda.Services
import           HLambda.Types
import           HLambda.Util
import           Network.AWS.Easy (connect)

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

    stsSession <- connect conf stsService
    accountId <- getAccountId stsSession
    logMessage $ "Account ID: " ++ show accountId

    ssmSession <- connect conf ssmService
    setStringParameter
        (ParameterName "/path/to/parameter")
        "hello-world"
        ssmSession

    clientInfo <- getSecureStringParameter (ParameterName "/HLambda/FitbitAPI/ClientInfo") ssmSession
    logMessage $ "clientInfo=" ++ show clientInfo

    print (event :: Value)
    return ([1, 2, 3] :: [Int])
