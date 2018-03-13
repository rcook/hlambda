module HLambda.Commands
    ( getAccountId
    , getSecureStringParameter
    , setStringParameter
    ) where

import           AWSViaHaskell (withAWS)
import           Control.Exception (throwIO)
import           Control.Lens ((&), (.~), (^.))
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import           HLambda.Errors
import           HLambda.Services
import           HLambda.Types
import           Network.AWS (send)
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
    case result ^. gcirsAccount of
        Just s -> return $ AccountId s
        Nothing -> liftIO $ throwIO (RuntimeError "Could not retrieve AWS account ID")

getSecureStringParameter :: ParameterName -> SSMSession -> IO Text
getSecureStringParameter (ParameterName pn) = withAWS $ do
    result <- send (getParameter pn & gWithDecryption .~ Just True)
    let mbValue = do
                    p <- result ^. gprsParameter
                    t <- p ^. pType
                    case t of
                        SecureString -> p ^. pValue
                        _ -> Nothing
    case mbValue of
        Nothing -> liftIO $ throwIO (RuntimeError $ "Could not retrieve secure string " ++ show pn)
        Just value -> return value

setStringParameter :: ParameterName -> Text -> SSMSession -> IO ()
setStringParameter (ParameterName pn) pv = withAWS $ do
    void $ send (putParameter pn pv String & ppOverwrite .~ Just True)
