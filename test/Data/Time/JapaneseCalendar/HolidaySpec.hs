module Data.Time.JapaneseCalendar.HolidaySpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Maybe
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

      , (fromGregorian 2016 1 1, Just NewYearsDay)
      , (fromGregorian 2016 1 11, Just ComingOfAgeDay)
      , (fromGregorian 2016 2 11, Just FoundationDay)
      , (fromGregorian 2016 3 20, Just VernalEquinoxDay)
      , (fromGregorian 2016 3 21, Just TransferHoliday)
      , (fromGregorian 2016 4 29, Just ShowaDay)
      , (fromGregorian 2016 5 3, Just ConstitutionMemorialDay)
      , (fromGregorian 2016 5 4, Just GreeneryDay)
      , (fromGregorian 2016 5 5, Just ChildrensDay)
      , (fromGregorian 2016 7 18, Just MarineDay)
      , (fromGregorian 2016 8 11, Just MountainDay)
      , (fromGregorian 2016 9 19, Just RespectForTheAgedDay)
      , (fromGregorian 2016 9 22, Just AutumnalEquinoxDay)
      , (fromGregorian 2016 10 10, Just HealthAndSportsDay)
      , (fromGregorian 2016 11 3, Just CultureDay)
      , (fromGregorian 2016 11 23, Just LabourThanksgivingDay)
      , (fromGregorian 2016 12 23, Just TheEmperorsBirthday)

      , (fromGregorian 2018 12 23, Just TheEmperorsBirthday)
      , (fromGregorian 2019 2 23, Nothing)
      , (fromGregorian 2019 12 23, Nothing)
      , (fromGregorian 2020 2 23, Just TheEmperorsBirthday)

      , (fromGregorian 2019 1 1, Just NewYearsDay)
      , (fromGregorian 2019 1 14, Just ComingOfAgeDay)
      , (fromGregorian 2019 2 11, Just FoundationDay)
      , (fromGregorian 2019 3 21, Just VernalEquinoxDay)
      , (fromGregorian 2019 4 29, Just ShowaDay)
      , (fromGregorian 2019 4 30, Just CitizensHoliday)
      , (fromGregorian 2019 5 1, Just ImperialEvent)
      , (fromGregorian 2019 5 2, Just CitizensHoliday)
      , (fromGregorian 2019 5 3, Just ConstitutionMemorialDay)
      , (fromGregorian 2019 5 4, Just GreeneryDay)
      , (fromGregorian 2019 5 5, Just ChildrensDay)
      , (fromGregorian 2019 5 6, Just TransferHoliday)
      , (fromGregorian 2019 7 15, Just MarineDay)
      , (fromGregorian 2019 8 11, Just MountainDay)
      , (fromGregorian 2019 8 12, Just TransferHoliday)
      , (fromGregorian 2019 9 16, Just RespectForTheAgedDay)
      , (fromGregorian 2019 9 23, Just AutumnalEquinoxDay)
      , (fromGregorian 2019 10 14, Just HealthAndSportsDay)
      , (fromGregorian 2019 10 22, Just ImperialEvent)
      , (fromGregorian 2019 11 3, Just CultureDay)
      , (fromGregorian 2019 11 4, Just TransferHoliday)
      , (fromGregorian 2019 11 23, Just LabourThanksgivingDay)

      , (fromGregorian 2020 1 1, Just NewYearsDay)
      , (fromGregorian 2020 1 13, Just ComingOfAgeDay)
      , (fromGregorian 2020 2 11, Just FoundationDay)
      , (fromGregorian 2020 2 23, Just TheEmperorsBirthday)
      , (fromGregorian 2020 2 24, Just TransferHoliday)
      , (fromGregorian 2020 3 20, Just VernalEquinoxDay)
      , (fromGregorian 2020 4 29, Just ShowaDay)
      , (fromGregorian 2020 5 3, Just ConstitutionMemorialDay)
      , (fromGregorian 2020 5 4, Just GreeneryDay)
      , (fromGregorian 2020 5 5, Just ChildrensDay)
      , (fromGregorian 2020 5 6, Just TransferHoliday)
      , (fromGregorian 2020 7 23, Just MarineDay)
      , (fromGregorian 2020 7 24, Just SportsDay)
      , (fromGregorian 2020 8 10, Just MountainDay)
      , (fromGregorian 2020 9 21, Just RespectForTheAgedDay)
      , (fromGregorian 2020 9 22, Just AutumnalEquinoxDay)
      , (fromGregorian 2020 11 3, Just CultureDay)
      , (fromGregorian 2020 11 23, Just LabourThanksgivingDay)

      , (fromGregorian 2021 1 1, Just NewYearsDay)
      , (fromGregorian 2021 1 11, Just ComingOfAgeDay)
      , (fromGregorian 2021 2 11, Just FoundationDay)
      , (fromGregorian 2021 2 23, Just TheEmperorsBirthday)
      , (fromGregorian 2021 3 20, Just VernalEquinoxDay)
      , (fromGregorian 2021 4 29, Just ShowaDay)
      , (fromGregorian 2021 5 3, Just ConstitutionMemorialDay)
      , (fromGregorian 2021 5 4, Just GreeneryDay)
      , (fromGregorian 2021 5 5, Just ChildrensDay)
      , (fromGregorian 2021 7 22, Just MarineDay)
      , (fromGregorian 2021 7 23, Just SportsDay)
      , (fromGregorian 2021 8 8, Just MountainDay)
      , (fromGregorian 2021 9 20, Just RespectForTheAgedDay)
      , (fromGregorian 2021 9 23, Just AutumnalEquinoxDay)
      , (fromGregorian 2021 11 3, Just CultureDay)
      , (fromGregorian 2021 11 23, Just LabourThanksgivingDay)

      , (fromGregorian 2022 1 1, Just NewYearsDay)
      , (fromGregorian 2022 1 10, Just ComingOfAgeDay)
      , (fromGregorian 2022 2 11, Just FoundationDay)
      , (fromGregorian 2022 2 23, Just TheEmperorsBirthday)
      , (fromGregorian 2022 3 21, Just VernalEquinoxDay)
      , (fromGregorian 2022 4 29, Just ShowaDay)
      , (fromGregorian 2022 5 3, Just ConstitutionMemorialDay)
      , (fromGregorian 2022 5 4, Just GreeneryDay)
      , (fromGregorian 2022 5 5, Just ChildrensDay)
      , (fromGregorian 2022 7 18, Just MarineDay)
      , (fromGregorian 2022 8 11, Just MountainDay)
      , (fromGregorian 2022 9 19, Just RespectForTheAgedDay)
      , (fromGregorian 2022 9 23, Just AutumnalEquinoxDay)
      , (fromGregorian 2022 10 10, Just SportsDay)
      , (fromGregorian 2022 11 3, Just CultureDay)
      , (fromGregorian 2022 11 23, Just LabourThanksgivingDay)
      ] $ \(day, holiday) ->
        it ("should return " ++ show holiday ++ " when given " ++ show day) $
          holidayType day `shouldBe` holiday

    forM_
      [ (1947, 0)
      , (1948, 3)
      , (1949, 9)
      , (2016, 17)
      , (2019, 22)
      , (2020, 18)
      , (2021, 17)
      , (2022, 16)
      ] $ \(year, holidayNum) -> do
        let
          days = [fromGregorian year 1 1 .. fromGregorian year 12 31]
          holidays = mapMaybe holidayType days
        it ("should return " ++ show holidayNum ++ " holidays in " ++ show year) $
          length holidays `shouldBe` holidayNum
