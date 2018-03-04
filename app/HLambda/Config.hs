{-# LANGUAGE OverloadedStrings #-}

module HLambda.Config
    ( getAWSConfig
    ) where

import           AWSViaHaskell (AWSConfig, Endpoint(..), awsConfig, awscCredentials)
import           Control.Exception (throwIO)
import           Control.Lens ((&), (.~))
import           HLambda.Errors
import           Network.AWS (Region(..))
import           Network.AWS.Auth (Credentials(..))
import           System.Environment (getEnv)

getAWSConfig :: IO AWSConfig
getAWSConfig = do
    region <- getEnv "AWS_REGION"
    if region /= "us-east-2"
        then throwIO $ RuntimeError ("Unsupported AWS region " ++ region)
        else return $ awsConfig (AWSRegion Ohio)
                        & awscCredentials .~ FromEnv
                                                "AWS_ACCESS_KEY_ID"
                                                "AWS_SECRET_ACCESS_KEY"
                                                (Just "AWS_SESSION_TOKEN")
                                                (Just "AWS_REGION")
