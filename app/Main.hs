{-# LANGUAGE OverloadedStrings #-}

module Main (main, main') where

import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           HLambdaApp.Commands
import           HLambdaApp.Config
import           HLambdaApp.FitbitAPI
import           HLambdaApp.Services
import           HLambdaApp.Types
import           HLambdaApp.Util
import           Network.AWS.Easy (connect)
import           Network.HTTP.Req
                    ( (/:)
                    , (=:)
                    , GET(..)
                    , NoReqBody(..)
                    , https
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )
import           S3Demo (s3Main)

main :: IO ()
main = s3Main

main' :: IO ()
main' = withLambda $ \event -> do
    let _ = event :: Value
    conf <- getAWSConfigFromEnv

    logMessage "hlambda: Perform web API request"
    v <- runReq def $
        req GET (https "httpbin.org" /: "get") NoReqBody jsonResponse $
                "aaa" =: ("bbb" :: Text) <>
                "ccc" =: ("ddd" :: Text)
    print (responseBody v :: Value)

    -- (2) Call AWS STS API
    stsSession <- connect conf stsService
    accountId <- getAccountId stsSession
    logMessage $ "hlambda: Fetched account ID " ++ show accountId

    -- (3) Call AWS SSM APIs
    ssmSession <- connect conf ssmService
    logMessage "hlambda: Set string parameter"
    setStringParameter
        (ParameterName "/path/to/parameter")
        "hello-world"
        ssmSession

    clientInfo <- getClientInfo ssmSession
    logMessage $ "hlambda: Fetch client info: " ++ show clientInfo

    logMessage "hlambda: Set token pair"
    setTokenPair
        (TokenPair (AccessToken "foo") (RefreshToken "bar"))
        ssmSession
