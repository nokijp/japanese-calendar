{-# LANGUAGE TemplateHaskell #-}

module Data.Time.JapaneseCalendar.Holiday
  ( HolidayType(..)
  , holidayType
  ) where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.SolarTerm

-- | Japanese holidays
data HolidayType =
    NewYearsDay  -- ^ 元日
  | ComingOfAgeDay  -- ^ 成人の日
  | FoundationDay  -- ^ 建国記念の日
  | VernalEquinoxDay  -- ^ 春分の日
  | ShowaDay  -- ^ 昭和の日
  | ConstitutionMemorialDay  -- ^ 憲法記念日
  | GreeneryDay  -- ^ みどりの日
  | ChildrensDay  -- ^ こどもの日
  | MarineDay  -- ^ 海の日
  | MountainDay  -- ^ 山の日
  | RespectForTheAgedDay  -- ^ 敬老の日
  | AutumnalEquinoxDay  -- ^ 秋分の日
  | HealthAndSportsDay  -- ^ 体育の日
  | CultureDay  -- ^ 文化の日
  | LabourThanksgivingDay  -- ^ 勤労感謝の日
  | TheEmperorsBirthday  -- ^ 天皇誕生日
  | TransferHoliday  -- ^ 振替休日
  | CitizensHoliday  -- ^ 国民の休日
    deriving (Show, Eq)

derivingJapaneseNameBoundedEnum ''HolidayType
  [ "元日"
  , "成人の日"
  , "建国記念の日"
  , "春分の日"
  , "昭和の日"
  , "憲法記念日"
  , "みどりの日"
  , "こどもの日"
  , "海の日"
  , "山の日"
  , "敬老の日"
  , "秋分の日"
  , "体育の日"
  , "文化の日"
  , "勤労感謝の日"
  , "天皇誕生日"
  , "振替休日"
  , "国民の休日"
  ]

-- | returns a holiday type of a specified day
holidayType :: Day -> Maybe HolidayType
holidayType day
  | day >= fromGregorian 2016 1 1 = holiday2016 day
  | day >= fromGregorian 2007 1 1 = holiday2007 day
  | day >= fromGregorian 2003 1 1 = holiday2003 day
  | day >= fromGregorian 2000 1 1 = holiday2000 day
  | day >= fromGregorian 1996 1 1 = holiday1996 day
  | day >= fromGregorian 1989 2 17 = holiday1989 day
  | day >= fromGregorian 1985 12 27 = holiday1985 day
  | day >= fromGregorian 1973 4 12 = holiday1973 day
  | day >= fromGregorian 1966 6 25 = holiday1966 day
  | day >= fromGregorian 1948 7 20 = holiday1948 day
  | otherwise = Nothing

holiday2016 :: Day -> Maybe HolidayType
holiday2016 day =
      findScheduledHoliday scheduledHolidayRules2016 day
  <|> transferHoliday2007 scheduledHolidayRules2016 day
  <|> citizensHoliday1985 scheduledHolidayRules2016 day

holiday2007 :: Day -> Maybe HolidayType
holiday2007 day =
      findScheduledHoliday scheduledHolidayRules2007 day
  <|> transferHoliday2007 scheduledHolidayRules2007 day
  <|> citizensHoliday1985 scheduledHolidayRules2007 day

holiday2003 :: Day -> Maybe HolidayType
holiday2003 day =
      findScheduledHoliday scheduledHolidayRules2003 day
  <|> transferHoliday1973 scheduledHolidayRules2003 day
  <|> citizensHoliday1985 scheduledHolidayRules2003 day

holiday2000 :: Day -> Maybe HolidayType
holiday2000 day =
      findScheduledHoliday scheduledHolidayRules2000 day
  <|> transferHoliday1973 scheduledHolidayRules2000 day
  <|> citizensHoliday1985 scheduledHolidayRules2000 day

holiday1996 :: Day -> Maybe HolidayType
holiday1996 day =
      findScheduledHoliday scheduledHolidayRules1996 day
  <|> transferHoliday1973 scheduledHolidayRules1996 day
  <|> citizensHoliday1985 scheduledHolidayRules1996 day

holiday1989 :: Day -> Maybe HolidayType
holiday1989 day =
      findScheduledHoliday scheduledHolidayRules1989 day
  <|> transferHoliday1973 scheduledHolidayRules1989 day
  <|> citizensHoliday1985 scheduledHolidayRules1989 day

holiday1985 :: Day -> Maybe HolidayType
holiday1985 day = holiday1973 day <|> citizensHoliday1985 scheduledHolidayRules1966 day

holiday1973 :: Day -> Maybe HolidayType
holiday1973 day = holiday1966 day <|> transferHoliday1973 scheduledHolidayRules1966 day

holiday1966 :: Day -> Maybe HolidayType
holiday1966 = findScheduledHoliday scheduledHolidayRules1966

holiday1948 :: Day -> Maybe HolidayType
holiday1948 = findScheduledHoliday scheduledHolidayRules1948

findScheduledHoliday :: [HolidayRule] -> Day -> Maybe HolidayType
findScheduledHoliday rules day = asum $ ruleToHoliday <$> rules
  where
    (_, dayMonth, dayDay) = toGregorian day
    ruleToHoliday (FixedDateRule ruleMonth ruleDay ruleHoliday) =
      if ruleMonth == dayMonth && ruleDay == dayDay
      then Just ruleHoliday
      else Nothing
    ruleToHoliday (DayOfWeekRule ruleMonth ruleDayOfWeek ruleWeekNumber ruleHoliday) =
      if ruleMonth == dayMonth && ruleDayOfWeek == dayOfWeek day && ruleWeekNumber == (dayDay - 1) `div` 7 + 1
      then Just ruleHoliday
      else Nothing
    ruleToHoliday (SolarTermRule ruleSolarTerm ruleHoliday) =
      if ((== ruleSolarTerm) <$> solarTerm jst day) == Just True
      then Just ruleHoliday
      else Nothing

transferHoliday2007 :: [HolidayRule] -> Day -> Maybe HolidayType
transferHoliday2007 rules day = if isTransferHoliday2007 rules day then Just TransferHoliday else Nothing

isTransferHoliday2007 :: [HolidayRule] -> Day -> Bool
isTransferHoliday2007 rules day = isYesterdayHoliday && (isYesterdaySunday || isTransferHoliday2007 rules yesterday)
  where
    isYesterdayHoliday = isJust $ findScheduledHoliday rules yesterday
    isYesterdaySunday = dayOfWeek yesterday == Sunday
    yesterday = addDays (-1) day

transferHoliday1973 :: [HolidayRule] -> Day -> Maybe HolidayType
transferHoliday1973 rules day =
  if dayOfWeek day == Monday
  then TransferHoliday <$ findScheduledHoliday rules (addDays (-1) day)
  else Nothing

citizensHoliday1985 :: [HolidayRule] -> Day -> Maybe HolidayType
citizensHoliday1985 rules day = CitizensHoliday
  <$ (findScheduledHoliday rules (addDays (-1) day) <* findScheduledHoliday rules (addDays 1 day))

data HolidayRule =
    FixedDateRule { _holidayRuleMonth :: Int, _holidayRuleDay :: Int, _holidayRuleHoliday :: HolidayType }
  | DayOfWeekRule { _holidayRuleMonth :: Int, _holidayRuleDayOfWeek :: DayOfWeek, _holidayRuleWeekNumber :: Int, _holidayRuleHoliday :: HolidayType }
  | SolarTermRule { _holidayRuleSolarTerm :: SolarTerm, _holidayRuleHoliday :: HolidayType }

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Enum)

dayOfWeek :: Day -> DayOfWeek
dayOfWeek day = let (_, _, w) = toWeekDate day in toEnum (w - 1)

scheduledHolidayRules2016 :: [HolidayRule]
scheduledHolidayRules2016 =
  [ FixedDateRule 1 1 NewYearsDay
  , DayOfWeekRule 1 Monday 2 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 ShowaDay
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 4 GreeneryDay
  , FixedDateRule 5 5 ChildrensDay
  , DayOfWeekRule 7 Monday 3 MarineDay
  , FixedDateRule 8 11 MountainDay
  , DayOfWeekRule 9 Monday 3 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , DayOfWeekRule 10 Monday 2 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  , FixedDateRule 12 23 TheEmperorsBirthday
  ]

scheduledHolidayRules2007 :: [HolidayRule]
scheduledHolidayRules2007 =
  [ FixedDateRule 1 1 NewYearsDay
  , DayOfWeekRule 1 Monday 2 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 ShowaDay
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 4 GreeneryDay
  , FixedDateRule 5 5 ChildrensDay
  , DayOfWeekRule 7 Monday 3 MarineDay
  , DayOfWeekRule 9 Monday 3 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , DayOfWeekRule 10 Monday 2 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  , FixedDateRule 12 23 TheEmperorsBirthday
  ]

scheduledHolidayRules2003 :: [HolidayRule]
scheduledHolidayRules2003 =
  [ FixedDateRule 1 1 NewYearsDay
  , DayOfWeekRule 1 Monday 2 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 GreeneryDay
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 5 ChildrensDay
  , DayOfWeekRule 7 Monday 3 MarineDay
  , DayOfWeekRule 9 Monday 3 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , DayOfWeekRule 10 Monday 2 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  , FixedDateRule 12 23 TheEmperorsBirthday
  ]

scheduledHolidayRules2000 :: [HolidayRule]
scheduledHolidayRules2000 =
  [ FixedDateRule 1 1 NewYearsDay
  , DayOfWeekRule 1 Monday 2 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 GreeneryDay
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 5 ChildrensDay
  , FixedDateRule 7 20 MarineDay
  , FixedDateRule 9 15 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , DayOfWeekRule 10 Monday 2 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  , FixedDateRule 12 23 TheEmperorsBirthday
  ]

scheduledHolidayRules1996 :: [HolidayRule]
scheduledHolidayRules1996 =
  [ FixedDateRule 1 1 NewYearsDay
  , FixedDateRule 1 15 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 GreeneryDay
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 5 ChildrensDay
  , FixedDateRule 7 20 MarineDay
  , FixedDateRule 9 15 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , FixedDateRule 10 10 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  , FixedDateRule 12 23 TheEmperorsBirthday
  ]

scheduledHolidayRules1989 :: [HolidayRule]
scheduledHolidayRules1989 =
  [ FixedDateRule 1 1 NewYearsDay
  , FixedDateRule 1 15 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 GreeneryDay
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 5 ChildrensDay
  , FixedDateRule 9 15 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , FixedDateRule 10 10 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  , FixedDateRule 12 23 TheEmperorsBirthday
  ]

scheduledHolidayRules1966 :: [HolidayRule]
scheduledHolidayRules1966 =
  [ FixedDateRule 1 1 NewYearsDay
  , FixedDateRule 1 15 ComingOfAgeDay
  , FixedDateRule 2 11 FoundationDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 TheEmperorsBirthday
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 5 ChildrensDay
  , FixedDateRule 9 15 RespectForTheAgedDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , FixedDateRule 10 10 HealthAndSportsDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  ]

scheduledHolidayRules1948 :: [HolidayRule]
scheduledHolidayRules1948 =
  [ FixedDateRule 1 1 NewYearsDay
  , FixedDateRule 1 15 ComingOfAgeDay
  , SolarTermRule VernalEquinox VernalEquinoxDay
  , FixedDateRule 4 29 TheEmperorsBirthday
  , FixedDateRule 5 3 ConstitutionMemorialDay
  , FixedDateRule 5 5 ChildrensDay
  , SolarTermRule AutumnalEquinox AutumnalEquinoxDay
  , FixedDateRule 11 3 CultureDay
  , FixedDateRule 11 23 LabourThanksgivingDay
  ]
