{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HLambda.Services
    ( SSMService
    , SSMSession
    , STSService
    , STSSession
    , ssmService
    , stsService
    ) where

import           AWSViaHaskell (wrapAWSService)
import           Network.AWS.SSM (ssm)
import           Network.AWS.STS (sts)

wrapAWSService 'ssm "SSMService" "SSMSession"
wrapAWSService 'sts "STSService" "STSSession"
