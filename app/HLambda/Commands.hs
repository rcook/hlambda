module HLambda.Commands
    ( getAccountId
    , setParameter
    ) where

import           AWSViaHaskell (withAWS)
import           Control.Exception (throwIO)
import           Control.Lens ((&), (.~), (^.))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           HLambda.Errors
import           HLambda.Services
import           HLambda.Types
import           Network.AWS (send)
import           Network.AWS.SSM (ParameterType(..), ppOverwrite, putParameter)
import           Network.AWS.STS (gcirsAccount, getCallerIdentity)

getAccountId :: STSSession -> IO AccountId
getAccountId = withAWS $ do
    result <- send getCallerIdentity
    case result ^. gcirsAccount of
        Just s -> return $ AccountId s
        Nothing -> liftIO $ throwIO (RuntimeError "Could not retrieve AWS account ID")

setParameter :: ParameterName -> ParameterValue -> SSMSession -> IO ()
setParameter (ParameterName pn) (ParameterString ps) = withAWS $ do
    void $ send (putParameter pn ps String & ppOverwrite .~ Just True)
