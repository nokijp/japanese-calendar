module Data.Time.JapaneseCalendar.Internal.JulianCenturySpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.JapaneseCalendar.Internal.JulianCentury
import Test.Hspec
import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "julianCenturyFromUTCTime" $ do
    forM_ julianCenturyAndUTCTime $ \(julianCentury, utcTime) ->
      it ("should return " ++ show utcTime ++ " when given " ++ show julianCentury) $
        julianCenturyFromUTCTime utcTime `shouldSatisfy` isAlmostEqual (1 / (36525 * 86400)) julianCentury

  describe "utcTimeFromJulianCentury" $ do
    forM_ julianCenturyAndUTCTime $ \(julianCentury, utcTime) ->
      it ("should return " ++ show julianCentury ++ " when given " ++ show utcTime) $
        utcTimeFromJulianCentury julianCentury `shouldSatisfy` isAlmostEqualUTCTime 1 utcTime

julianCenturyAndUTCTime :: [(Double, UTCTime)]
julianCenturyAndUTCTime =
  [ (0.0, UTCTime (fromGregorian 2000 1 1) (picosecondsToDiffTime $ (43200000 - 64184) * 1000000000))
  , ((32.184 + 32) / (36525 * 86400), UTCTime (fromGregorian 2000 1 1) 43200)
  , (1.0 + (32.184 + 37) / (36525 * 86400), UTCTime (fromGregorian 2100 1 1) 43200)
  ]
