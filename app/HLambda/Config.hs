{-# LANGUAGE OverloadedStrings #-}

module HLambda.Config
    ( getAWSConfigFromEnv
    , getEnvRegion
    , getAWSConfig
    ) where

import           Control.Lens ((&), (.~))
import           Data.String (IsString)
import qualified Data.Text as Text (pack)
import           HLambda.Errors
import           HLambda.Util
import           Network.AWS (Region)
import           Network.AWS.Auth (AccessKey, Credentials(..), SecretKey)
import           Network.AWS.Data (fromText)
import           Network.AWS.Easy (AWSConfig, Endpoint(..), awsConfig, awscCredentials)
import           System.Environment (getEnv)

awsAccessKeyIdName :: IsString s => s
awsAccessKeyIdName = "AWS_ACCESS_KEY_ID"

awsSecretAccessKeyName :: IsString s => s
awsSecretAccessKeyName = "AWS_SECRET_ACCESS_KEY"

awsSessionTokenName :: IsString s => s
awsSessionTokenName = "AWS_SESSION_TOKEN"

awsRegionName :: IsString s => s
awsRegionName = "AWS_REGION"

getEnvRegion :: String -> IO Region
getEnvRegion regionName = do
    regionStr <- getEnv regionName
    fromRightIO
        (\e -> RuntimeError ("Could not parse AWS region " ++ regionStr ++ ": " ++ e))
        (fromText (Text.pack regionStr))

getAWSConfigFromEnv :: IO AWSConfig
getAWSConfigFromEnv = do
    region <- getEnvRegion awsRegionName
    return $ awsConfig (AWSRegion region)
                & awscCredentials .~ FromEnv
                                        awsAccessKeyIdName
                                        awsSecretAccessKeyName
                                        (Just awsSessionTokenName)
                                        (Just awsRegionName)

getAWSConfig :: Region -> AccessKey -> SecretKey -> AWSConfig
getAWSConfig region ak sk = awsConfig (AWSRegion region)
                                & awscCredentials .~ FromKeys ak sk
