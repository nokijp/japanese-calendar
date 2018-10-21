module TestUtils
  ( timeFromYMDHMS
  , isAlmostEqual
  , isAlmostEqualUTCTime
  ) where

import Data.Time.Clock
import Data.Time.Calendar

timeFromYMDHMS :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
timeFromYMDHMS year month day hour minute second = UTCTime (fromGregorian year month day) (fromIntegral $ hour * 60 * 60 + minute * 60 + second)

isAlmostEqual :: (Num a, Ord a) => a -> a -> a -> Bool
isAlmostEqual tolerance a b = abs (a - b) < tolerance

isAlmostEqualUTCTime :: Integer -> UTCTime -> UTCTime -> Bool
isAlmostEqualUTCTime toleranceInSeconds a b = abs (diffUTCTime a b) < fromIntegral toleranceInSeconds
