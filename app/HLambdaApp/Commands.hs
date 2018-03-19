module HLambdaApp.Commands
    ( getAccountId
    , getSecureStringParameter
    , getStringParameter
    , setSecureStringParameter
    , setStringParameter
    ) where

import           Control.Exception (throwIO)
import           Control.Lens ((&), (.~), (^.))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import           HLambdaApp.Errors
import           HLambdaApp.Services
import           HLambdaApp.Types
import           HLambdaApp.Util
import           Network.AWS (send)
import           Network.AWS.Easy (withAWS)
import           Network.AWS.SSM
                    ( ParameterType(..)
                    , gWithDecryption
                    , getParameter
                    , gprsParameter
                    , ppOverwrite
                    , pType
                    , putParameter
                    , pValue
                    )
import           Network.AWS.STS (gcirsAccount, getCallerIdentity)

getAccountId :: STSSession -> IO AccountId
getAccountId = withAWS $ do
    result <- send getCallerIdentity
    AccountId <$> fromJustIO (RuntimeError "Could not retrieve AWS account ID") (result ^. gcirsAccount)

getSecureStringParameter :: ParameterName -> SSMSession -> IO Text
getSecureStringParameter (ParameterName pn) = withAWS $ do
    result <- send (getParameter pn & gWithDecryption .~ Just True)
    p <- fromJustIO (RuntimeError $ "Could not get parameter " ++ show pn) (result ^. gprsParameter)
    t <- fromJustIO (RuntimeError $ "Could not get type for parameter " ++ show pn) (p ^. pType)
    case t of
        SecureString -> fromJustIO (RuntimeError $ "Could not get value for parameter " ++ show pn) (p ^. pValue)
        _ -> liftIO (throwIO (RuntimeError $ "Unxpected parameter type for parameter " ++ show pn))

getStringParameter :: ParameterName -> SSMSession -> IO Text
getStringParameter (ParameterName pn) = withAWS $ do
    result <- send (getParameter pn)
    p <- fromJustIO (RuntimeError $ "Could not get parameter " ++ show pn) (result ^. gprsParameter)
    t <- fromJustIO (RuntimeError $ "Could not get type for parameter " ++ show pn) (p ^. pType)
    case t of
        String -> fromJustIO (RuntimeError $ "Could not get value for parameter " ++ show pn) (p ^. pValue)
        _ -> liftIO (throwIO (RuntimeError $ "Unxpected parameter type for parameter " ++ show pn))

setSecureStringParameter :: ParameterName -> Text -> SSMSession -> IO ()
setSecureStringParameter (ParameterName pn) s = withAWS $ do
    void $ send (putParameter pn s SecureString & ppOverwrite .~ Just True)

setStringParameter :: ParameterName -> Text -> SSMSession -> IO ()
setStringParameter (ParameterName pn) pv = withAWS $ do
    void $ send (putParameter pn pv String & ppOverwrite .~ Just True)
