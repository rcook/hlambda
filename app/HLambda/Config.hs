{-# LANGUAGE OverloadedStrings #-}

module HLambda.Config
    ( getAWSConfig
    ) where

import           AWSViaHaskell (AWSConfig, Endpoint(..), awsConfig, awscCredentials)
import           Control.Exception (throwIO)
import           Control.Lens ((&), (.~))
import qualified Data.Text as Text (pack)
import           HLambda.Errors
import           Network.AWS.Auth (Credentials(..))
import           Network.AWS.Data (fromText)
import           System.Environment (getEnv)

getAWSConfig :: IO AWSConfig
getAWSConfig = do
    regionStr <- getEnv "AWS_REGION"
    region <- case fromText (Text.pack regionStr) of
                Left s -> throwIO (RuntimeError $ "Unrecognized region " ++ regionStr ++ ": " ++ s)
                Right r -> return r
    return $ awsConfig (AWSRegion region)
                & awscCredentials .~ FromEnv
                                        "AWS_ACCESS_KEY_ID"
                                        "AWS_SECRET_ACCESS_KEY"
                                        (Just "AWS_SESSION_TOKEN")
                                        (Just "AWS_REGION")
