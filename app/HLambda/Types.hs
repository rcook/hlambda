module HLambda.Types
    ( AccountId(..)
    , ParameterName(..)
    , ParameterValue(..)
    ) where

import           Data.Text (Text)

newtype AccountId = AccountId Text deriving Show
newtype ParameterName = ParameterName Text deriving Show
newtype ParameterValue = ParameterString Text deriving Show
