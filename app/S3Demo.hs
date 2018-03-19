{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module S3Demo (s3Main) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           HLambda (formatTimeISO8601)
import           Network.AWS.Easy
import           Network.AWS.DynamoDB
import           Network.AWS.S3

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
wrapAWSService 's3 "S3Service" "S3Session"

type ContinuationToken = Maybe Text
newtype TableName = TableName Text deriving Show
data ObjectInfo = ObjectInfo ObjectKey UTCTime deriving Show

getObjectInfos :: BucketName -> S3Session -> IO [ObjectInfo]
getObjectInfos bn s3Session = getObjectInfosHelper bn Nothing s3Session

getObjectInfosHelper :: BucketName -> ContinuationToken -> S3Session -> IO [ObjectInfo]
getObjectInfosHelper bn ct s3Session = (flip withAWS) s3Session $ do
    result <- send $ listObjectsV bn & lContinuationToken .~ ct
    let keys = map (\x -> ObjectInfo (x ^. oKey) (x ^. oLastModified)) (result ^. lrsContents)
        nextCT = result ^. lrsNextContinuationToken
    case nextCT of
        Nothing -> return keys
        Just _ -> do
            nextKeys <- liftIO $ getObjectInfosHelper bn nextCT s3Session
            return $ keys ++ nextKeys

putObjectInfo :: TableName -> ObjectInfo -> DynamoDBSession -> IO ()
putObjectInfo (TableName tn) (ObjectInfo key lastModified) = withAWS $ do
    void $ send $ putItem tn & piItem .~ item
    where item = HashMap.fromList
            [ ("key", attributeValue & avS .~ Just (Text.pack $ show key))
            , ("last-modified", attributeValue & avN .~ Just (formatTimeISO8601 lastModified))
            ]

s3Main :: IO ()
s3Main = do
    let dynamoDBConfig = awsConfig (Local "localhost" 4569)
    dynamoDBSession <- connect dynamoDBConfig dynamoDBService

    let s3Config = awsConfig (Local "localhost" 4572)
    s3Session <- connect s3Config s3Service
    objectInfos <- getObjectInfos (BucketName "my-bucket") s3Session

    for_ objectInfos $ \objectInfo ->
        putObjectInfo (TableName "objects") objectInfo dynamoDBSession
