module Data.Time.JapaneseCalendar.HolidaySpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Holiday
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "holidayType" $ do
    forM_
      [ (fromGregorian 1948 1 1, Nothing)
      , (fromGregorian 1949 1 1, Just NewYearsDay)
      , (fromGregorian 1949 1 2, Nothing)
      , (fromGregorian 1967 1 1, Just NewYearsDay)
      , (fromGregorian 1974 1 1, Just NewYearsDay)
      , (fromGregorian 1986 1 1, Just NewYearsDay)
      , (fromGregorian 1990 1 1, Just NewYearsDay)
      , (fromGregorian 1996 1 1, Just NewYearsDay)
      , (fromGregorian 2000 1 1, Just NewYearsDay)
      , (fromGregorian 2003 1 1, Just NewYearsDay)
      , (fromGregorian 2007 1 1, Just NewYearsDay)

      , (fromGregorian 1987 5 4, Just TransferHoliday)
      , (fromGregorian 1988 5 4, Just CitizensHoliday)
      , (fromGregorian 2008 5 6, Just TransferHoliday)
      , (fromGregorian 2009 9 22, Just CitizensHoliday)
      , (fromGregorian 2009 9 24, Nothing)

      , (fromGregorian 2015 12 31, Nothing)
      , (fromGregorian 2016 1 1, Just NewYearsDay)
      , (fromGregorian 2016 1 2, Nothing)
      , (fromGregorian 2016 1 10, Nothing)
      , (fromGregorian 2016 1 11, Just ComingOfAgeDay)
      , (fromGregorian 2016 1 12, Nothing)
      , (fromGregorian 2016 2 10, Nothing)
      , (fromGregorian 2016 2 11, Just FoundationDay)
      , (fromGregorian 2016 2 12, Nothing)
      , (fromGregorian 2016 3 19, Nothing)
      , (fromGregorian 2016 3 20, Just VernalEquinoxDay)
      , (fromGregorian 2016 3 21, Just TransferHoliday)
      , (fromGregorian 2016 3 22, Nothing)
      , (fromGregorian 2016 4 28, Nothing)
      , (fromGregorian 2016 4 29, Just ShowaDay)
      , (fromGregorian 2016 4 30, Nothing)
      , (fromGregorian 2016 5 2, Nothing)
      , (fromGregorian 2016 5 3, Just ConstitutionMemorialDay)
      , (fromGregorian 2016 5 4, Just GreeneryDay)
      , (fromGregorian 2016 5 5, Just ChildrensDay)
      , (fromGregorian 2016 5 6, Nothing)
      , (fromGregorian 2016 7 17, Nothing)
      , (fromGregorian 2016 7 18, Just MarineDay)
      , (fromGregorian 2016 7 19, Nothing)
      , (fromGregorian 2016 8 10, Nothing)
      , (fromGregorian 2016 8 11, Just MountainDay)
      , (fromGregorian 2016 8 12, Nothing)
      , (fromGregorian 2016 9 18, Nothing)
      , (fromGregorian 2016 9 19, Just RespectForTheAgedDay)
      , (fromGregorian 2016 9 20, Nothing)
      , (fromGregorian 2016 9 21, Nothing)
      , (fromGregorian 2016 9 22, Just AutumnalEquinoxDay)
      , (fromGregorian 2016 9 23, Nothing)
      , (fromGregorian 2016 10 9, Nothing)
      , (fromGregorian 2016 10 10, Just HealthAndSportsDay)
      , (fromGregorian 2016 10 11, Nothing)
      , (fromGregorian 2016 11 2, Nothing)
      , (fromGregorian 2016 11 3, Just CultureDay)
      , (fromGregorian 2016 11 4, Nothing)
      , (fromGregorian 2016 11 22, Nothing)
      , (fromGregorian 2016 11 23, Just LabourThanksgivingDay)
      , (fromGregorian 2016 11 24, Nothing)
      , (fromGregorian 2016 12 22, Nothing)
      , (fromGregorian 2016 12 23, Just TheEmperorsBirthday)
      , (fromGregorian 2016 12 24, Nothing)

      , (fromGregorian 2018 12 23, Just TheEmperorsBirthday)
      , (fromGregorian 2019 2 23, Nothing)
      , (fromGregorian 2019 12 23, Nothing)
      , (fromGregorian 2020 2 23, Just TheEmperorsBirthday)
      ] $ \(day, holiday) ->
        it ("should return " ++ show holiday ++ " when given " ++ show day) $
          holidayType day `shouldBe` holiday
