module Data.Time.JapaneseCalendar.Internal.MoonSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.JapaneseCalendar.Internal.Moon
import TestUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nearestNewMoon" $ do
    forM_
      [ (timeFromYMDHMS 2000 1 1 0 0 0, timeFromYMDHMS 2000 1 6 18 14 0)
      , (timeFromYMDHMS 2000 1 10 0 0 0, timeFromYMDHMS 2000 1 6 18 14 0)
      , (timeFromYMDHMS 2000 2 1 0 0 0, timeFromYMDHMS 2000 2 5 13 3 0)
      , (timeFromYMDHMS 2000 2 10 0 0 0, timeFromYMDHMS 2000 2 5 13 3 0)

      , (timeFromYMDHMS 2010 1 10 0 0 0, timeFromYMDHMS 2010 1 15 7 11 0)
      , (timeFromYMDHMS 2010 1 20 0 0 0, timeFromYMDHMS 2010 1 15 7 11 0)
      , (timeFromYMDHMS 2010 2 10 0 0 0, timeFromYMDHMS 2010 2 14 2 51 0)
      , (timeFromYMDHMS 2010 2 20 0 0 0, timeFromYMDHMS 2010 2 14 2 51 0)
      ] $ \(inputTime, expectedTime) ->
        it ("should return " ++ show expectedTime ++ " when given " ++ show inputTime) $
          nearestNewMoon inputTime `shouldSatisfy` (isAlmostEqual (2 * 60) expectedTime)
