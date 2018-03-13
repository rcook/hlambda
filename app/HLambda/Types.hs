module HLambda.Types
    ( AccountId(..)
    , ParameterName(..)
    ) where

import           Data.Text (Text)

newtype AccountId = AccountId Text deriving Show
newtype ParameterName = ParameterName Text deriving Show
