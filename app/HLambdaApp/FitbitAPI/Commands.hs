{-# LANGUAGE OverloadedStrings #-}

module HLambdaApp.FitbitAPI.Commands
    ( getClientInfo
    , getTokenPair
    , setTokenPair
    ) where

import           Control.Exception (throwIO)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text (splitOn)
import           HLambdaApp.Commands
import           HLambdaApp.Errors
import           HLambdaApp.FitbitAPI.Types
import           HLambdaApp.Services
import           HLambdaApp.Types

clientInfoName :: ParameterName
clientInfoName = ParameterName "/HLambda/FitbitAPI/ClientInfo"

tokenPairName :: ParameterName
tokenPairName = ParameterName "/HLambda/FitbitAPI/TokenPair"

pPair :: Text -> Maybe (Text, Text)
pPair s =
    case Text.splitOn ";" s of
        s1 : s2 : [] -> return (s1, s2)
        _ -> Nothing

pHelper :: (a -> b -> c) -> (Text -> a) -> (Text -> b) -> Text -> Maybe c
pHelper c0 c1 c2 s = (\(s1, s2) -> c0 (c1 s1) (c2 s2)) <$> (pPair s)

pClientInfo :: Text -> Maybe ClientInfo
pClientInfo = pHelper ClientInfo ClientId ClientSecret

pTokenPair :: Text -> Maybe TokenPair
pTokenPair = pHelper TokenPair AccessToken RefreshToken

getPairHelper :: String -> ParameterName -> (Text -> Maybe a) -> SSMSession -> IO a
getPairHelper label parameterName p ssmSession = do
    s <- getSecureStringParameter parameterName ssmSession
    case p s of
        Nothing -> throwIO $ RuntimeError ("Could not parse " ++ label)
        Just result -> return result

getClientInfo :: SSMSession -> IO ClientInfo
getClientInfo = getPairHelper "Fitbit API info" clientInfoName pClientInfo

getTokenPair :: SSMSession -> IO TokenPair
getTokenPair = getPairHelper "token pair" tokenPairName pTokenPair

setTokenPair :: TokenPair -> SSMSession -> IO ()
setTokenPair (TokenPair (AccessToken at) (RefreshToken rt)) ssmSession =
    let s = at <> ";" <> rt
    in setSecureStringParameter tokenPairName s ssmSession
