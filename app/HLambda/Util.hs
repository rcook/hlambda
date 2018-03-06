module HLambda.Util
    ( fromJustIO
    , fromRightIO
    , logMessage
    , withLambda
    ) where

import           AWSLambda (lambdaMain)
import           Control.Exception
                    ( Exception
                    , SomeException
                    , bracket_
                    , catch
                    , throwIO
                    )
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson (Value)
import           System.IO (hFlush, stdout)

withLambda :: (Value -> IO ()) -> IO ()
withLambda body =
    bracket_
        (logMessage "hlambda started")
        (logMessage "hlambda stopped")
        (lambdaMain body)
        `catch` (\e -> logMessage $ "Unhandled exception: " ++ show (e :: SomeException))

logMessage :: MonadIO m => String -> m ()
logMessage s = liftIO (putStrLn s >> hFlush stdout)

fromJustIO :: (MonadIO m, Exception e) => e -> Maybe a -> m a
fromJustIO _ (Just a) = pure a
fromJustIO e _ = liftIO $ throwIO e

fromRightIO :: (MonadIO m, Exception e) => (b -> e) -> Either b a -> m a
fromRightIO _ (Right a) = pure a
fromRightIO f (Left e) = liftIO $ throwIO (f e)
