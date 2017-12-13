module TestUtils
  ( timeFromYMDHMS
  , isAlmostEqual
  ) where

import Data.Time.Clock
import Data.Time.Calendar

timeFromYMDHMS :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
timeFromYMDHMS year month day hour minute second = UTCTime (fromGregorian year month day) (fromIntegral $ hour * 60 * 60 + minute * 60 + second)

isAlmostEqual :: Integer -> UTCTime -> UTCTime -> Bool
isAlmostEqual toleranceInSeconds a b = abs (diffUTCTime a b) < fromIntegral toleranceInSeconds
