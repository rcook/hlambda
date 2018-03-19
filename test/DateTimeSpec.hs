{-|
Module      : DateTimeSpec
Description : Tests for @HLambda.DateTime@ module
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Allow orphan instance Arbitrary UTCTime

module DateTimeSpec (spec) where

import           Data.Text (Text)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..), picosecondsToDiffTime)
import           HLambda.DateTime (formatTimeISO8601, parseTimeISO8601)
import           Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, property)

-- https://gist.github.com/agrafix/2b48ec069693e3ab851e
instance Arbitrary UTCTime where
    arbitrary = do
        randomDay <- choose (1, 29)
        randomMonth <- choose (1, 12)
        randomYear <- choose (1900, 2100)
        randomTime <- choose (0, 86401) :: Gen Int
        return $ UTCTime
                    (fromGregorian randomYear randomMonth randomDay)
                    (fromIntegral randomTime)

checkFormatAndParse :: UTCTime -> Text -> Expectation
checkFormatAndParse utcTime expected = do
    let s = formatTimeISO8601 utcTime
    parseTimeISO8601 s `shouldBe` (Just utcTime)
    s `shouldBe` expected

spec :: Spec
spec = do
    describe "formatTimeISO8601" $ do
        it "should render no fractional seconds" $
            checkFormatAndParse
                (UTCTime (fromGregorian 2018 1 1) (picosecondsToDiffTime 0))
                "2018-01-01T00:00:00Z"
        it "should render picoseconds" $
            checkFormatAndParse
                (UTCTime (fromGregorian 2018 1 1) (picosecondsToDiffTime 123456))
                "2018-01-01T00:00:00.000000123456Z"
        it "should render six decimal places" $
            checkFormatAndParse
                (UTCTime (fromGregorian 2018 1 1) (picosecondsToDiffTime 123456000000))
                "2018-01-01T00:00:00.123456Z"
        it "should render three decimal places" $
            checkFormatAndParse
                (UTCTime (fromGregorian 2018 1 1) (picosecondsToDiffTime 123000000000))
                "2018-01-01T00:00:00.123Z"
        it "should render one decimal place" $
            checkFormatAndParse
                (UTCTime (fromGregorian 2018 1 1) (picosecondsToDiffTime 100000000000))
                "2018-01-01T00:00:00.1Z"
        it "should be inverse to parseTimeISO8601" $ property $
            \x -> (parseTimeISO8601 . formatTimeISO8601) x == Just x
