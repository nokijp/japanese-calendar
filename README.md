# Japanese Calendar

[![Build Status](https://travis-ci.org/nokijp/japanese-calendar.svg?branch=master)](https://travis-ci.org/nokijp/japanese-calendar)

This is a library for Haskell that provides functions to deal with the Japanese calendar.

[The haddocks are available here.](https://nokijp.github.io/japanese-calendar/)

This library contains:

- Tempo calendar (天保暦)
- Rokuyo (六曜)
- the twenty-four solar terms (二十四節気)
- Japanese public holidays (国民の祝日)

```haskell
import Data.Time.Calendar
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.Holiday
import Data.Time.JapaneseCalendar.Rokuyo
import Data.Time.JapaneseCalendar.SolarTerm
import Data.Time.JapaneseCalendar.TempoCalendar

main :: IO ()
main = do
  let tempoDate20010523 = tempoDate jst $ fromGregorian 2001 5 23 :: Maybe TempoDate
  print $ formatTempoDate <$> tempoDate20010523  -- Just "2001年 閏卯月 1日"

  let rokuyo20000614 = rokuyo jst $ fromGregorian 2000 6 14 :: Maybe Rokuyo
  print rokuyo20000614  -- Just Taian
  print $ rokuyoToJapaneseName <$> rokuyo20000614  -- Just "大安"

  let solarTerm20000320 = solarTerm jst $ fromGregorian 2000 3 20 :: Maybe SolarTerm
  print solarTerm20000320  -- Just VernalEquinox
  print $ solarTermToJapaneseName <$> solarTerm20000320  -- Just "春分"

  let holiday20001103 = holidayType $ fromGregorian 2000 11 3 :: Maybe HolidayType
  print holiday20001103  -- Just CultureDay
  print $ holidayName <$> holiday20001103  -- Just "文化の日"

formatTempoDate :: TempoDate -> String
formatTempoDate (TempoDate year month day) = show year ++ "年 " ++ tempoMonthToJapaneseName month ++ " " ++ show day ++ "日"
```
