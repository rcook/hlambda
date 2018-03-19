module HLambdaApp.FitbitAPI.Types
    ( AccessToken(..)
    , ClientId(..)
    , ClientInfo(..)
    , ClientSecret(..)
    , RefreshToken(..)
    , TokenPair(..)
    ) where

import           Data.Text (Text)

newtype ClientId = ClientId Text deriving Show

newtype ClientSecret = ClientSecret Text deriving Show

data ClientInfo = ClientInfo ClientId ClientSecret deriving Show

newtype AccessToken = AccessToken Text deriving Show

newtype RefreshToken = RefreshToken Text deriving Show

data TokenPair = TokenPair AccessToken RefreshToken deriving Show
