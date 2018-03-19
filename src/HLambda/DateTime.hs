{-|
Module      : HLambda.DateTime
Description : Date/time helper functions
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

A handful of date/time helper functions.
-}

{-# LANGUAGE OverloadedStrings #-}

module HLambda.DateTime
    ( formatTimeISO8601
    , parseTimeISO8601
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat, parseTimeM)

-- | Render a @UTCTime@ value into a (reasonably) standard ISO8601 format
-- The property @\x -> (parseTimeISO8601 . formatTimeISO8601) x == Just x@ should hold.
formatTimeISO8601 :: UTCTime -> Text
formatTimeISO8601 utcTime = Text.pack $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ") utcTime

-- | Parse a (reasonably) standard ISO8601 format into a @UTCTime@ value
-- The property @\x -> (parseTimeISO8601 . formatTimeISO8601) x == Just x@ should hold.
parseTimeISO8601 :: Text -> Maybe UTCTime
parseTimeISO8601 s = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (Text.unpack s)
