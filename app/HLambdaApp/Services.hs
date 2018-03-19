{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HLambdaApp.Services
    ( SSMService
    , SSMSession
    , STSService
    , STSSession
    , ssmService
    , stsService
    ) where

import           Network.AWS.Easy (wrapAWSService)
import           Network.AWS.SSM (ssm)
import           Network.AWS.STS (sts)

wrapAWSService 'ssm "SSMService" "SSMSession"
wrapAWSService 'sts "STSService" "STSSession"
