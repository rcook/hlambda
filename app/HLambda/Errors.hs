module HLambda.Errors
    ( RuntimeError(..)
    ) where

import           Control.Exception (Exception)

data RuntimeError = RuntimeError String deriving Show
instance Exception RuntimeError
