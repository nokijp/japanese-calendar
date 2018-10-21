module Data.Time.JapaneseCalendar.Internal.SunSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.JapaneseCalendar.Internal.Sun
import TestUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sunEclipticLongitudeToTime" $ do
    forM_
      [ (0, timeFromYMDHMS 2000 3 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)
      , (0, timeFromYMDHMS 2000 4 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)
      , (90, timeFromYMDHMS 2000 6 1 0 0 0, timeFromYMDHMS 2000 6 21 1 48 0)
      , (90, timeFromYMDHMS 2000 7 1 0 0 0, timeFromYMDHMS 2000 6 21 1 48 0)
      , (180, timeFromYMDHMS 2000 9 1 0 0 0, timeFromYMDHMS 2000 9 22 17 28 0)
      , (180, timeFromYMDHMS 2000 10 1 0 0 0, timeFromYMDHMS 2000 9 22 17 28 0)
      , (270, timeFromYMDHMS 2000 12 1 0 0 0, timeFromYMDHMS 2000 12 21 13 37 0)
      , (270, timeFromYMDHMS 2001 1 1 0 0 0, timeFromYMDHMS 2000 12 21 13 37 0)
      , (360, timeFromYMDHMS 2000 3 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)
      , (360, timeFromYMDHMS 2000 4 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)

      , (0, timeFromYMDHMS 2010 3 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      , (0, timeFromYMDHMS 2010 4 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      , (90, timeFromYMDHMS 2010 6 1 0 0 0, timeFromYMDHMS 2010 6 21 11 28 0)
      , (90, timeFromYMDHMS 2010 7 1 0 0 0, timeFromYMDHMS 2010 6 21 11 28 0)
      , (180, timeFromYMDHMS 2010 9 1 0 0 0, timeFromYMDHMS 2010 9 23 3 9 0)
      , (180, timeFromYMDHMS 2010 10 1 0 0 0, timeFromYMDHMS 2010 9 23 3 9 0)
      , (270, timeFromYMDHMS 2010 12 1 0 0 0, timeFromYMDHMS 2010 12 21 23 38 0)
      , (270, timeFromYMDHMS 2011 1 1 0 0 0, timeFromYMDHMS 2010 12 21 23 38 0)
      , (360, timeFromYMDHMS 2010 3 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      , (360, timeFromYMDHMS 2010 4 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      ] $ \(longitude, inputTime, expectedTime) ->
        it ("should return " ++ show expectedTime ++ " when given " ++ show (longitude, inputTime)) $
          sunEclipticLongitudeToTime longitude inputTime `shouldSatisfy` isAlmostEqualUTCTime (10 * 60) expectedTime
