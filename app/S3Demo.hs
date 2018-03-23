{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module S3Demo (s3Main) where

import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import           Data.Time
import           HLambda (formatTimeISO8601)
import           Network.AWS.Easy
import           Network.AWS.DynamoDB
import           Network.AWS.S3

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
wrapAWSService 's3 "S3Service" "S3Session"

newtype TableName = TableName Text deriving Show

data ObjectInfo = ObjectInfo ObjectKey UTCTime ETag deriving Show

getObjectInfos :: BucketName -> S3Session -> IO [ObjectInfo]
getObjectInfos bucketName s3Session = concat <$> go Nothing
    where
        go :: Maybe Text -> IO [[ObjectInfo]]
        go ct = do
            result <- (flip withAWS) s3Session $
                        send $ listObjectsV bucketName & lContinuationToken .~ ct
            let objectInfos = map
                                (\x -> ObjectInfo (x ^. oKey) (x ^. oLastModified) (x ^. oETag))
                                (result ^. lrsContents)
                nextCT = result ^. lrsNextContinuationToken
            case nextCT of
                Nothing -> return [objectInfos]
                Just _ -> do
                    nextObjectInfos <- go nextCT
                    return $ objectInfos : nextObjectInfos

getObjectInfosCPS :: BucketName -> S3Session -> IO [ObjectInfo]
getObjectInfosCPS bucketName s3Session = concat <$> go Nothing id
    where
        go :: Maybe Text -> ([[ObjectInfo]] -> [[ObjectInfo]]) -> IO [[ObjectInfo]]
        go ct f = do
            result <- (flip withAWS) s3Session $
                        send $ listObjectsV bucketName & lContinuationToken .~ ct
            let objectInfos = map
                                (\x -> ObjectInfo (x ^. oKey) (x ^. oLastModified) (x ^. oETag))
                                (result ^. lrsContents)
                nextCT = result ^. lrsNextContinuationToken
            case nextCT of
                Nothing -> return $ f [objectInfos]
                Just _ -> go nextCT (\nextObjectInfos -> f $ objectInfos : nextObjectInfos)

putObjectInfo :: TableName -> ObjectInfo -> DynamoDBSession -> IO ()
putObjectInfo (TableName tn) (ObjectInfo key lastModified eTag) = withAWS $ do
    void $ send $ putItem tn & piItem .~ item
    where item = HashMap.fromList
            [ ("key", attributeValue & avS .~ Just (toText key))
            , ("last-modified", attributeValue & avS .~ Just (formatTimeISO8601 lastModified))
            , ("etag", attributeValue & avS .~ Just (toText eTag))
            ]

s3Main :: IO ()
s3Main = do
    let dynamoDBConfig = awsConfig (Local "localhost" 4569)
    dynamoDBSession <- connect dynamoDBConfig dynamoDBService

    let s3Config = awsConfig (Local "localhost" 4572)
    s3Session <- connect s3Config s3Service
    objectInfos <- getObjectInfosCPS (BucketName "my-bucket") s3Session

    --for_ objectInfos $ \objectInfo ->
    --    putObjectInfo (TableName "my-table") objectInfo dynamoDBSession
    for_ objectInfos $ \objectInfo ->
        print objectInfo