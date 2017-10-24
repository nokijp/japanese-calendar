# Japanese Calendar

This is a library for Haskell that provides functions to deal with the Japanese calendar.

This library contains:

- Tempo calendar (天保暦)
- Rokuyo (六曜)
- the twenty-four solar terms (二十四節気)
- Japanese public holidays (国民の祝日)

```haskell
import Data.Maybe
import Data.Time.Calendar
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.Holiday
import Data.Time.JapaneseCalendar.Rokuyo
import Data.Time.JapaneseCalendar.SolarTerm
import Data.Time.JapaneseCalendar.TempoCalendar

main :: IO ()
main = do
  let tempoDate = tempoDateFromGregorian jst $ fromGregorian 2000 6 14 :: Maybe TempoDate
  let tempoDateString = formatTempoDate <$> tempoDate
  putStrLn $ fromMaybe "" tempoDateString  -- 2000年 皐月 13日

  let rokuyo = rokuyoFromTempoDate <$> tempoDate :: Maybe Rokuyo
  let rokuyoString = rokuyoToJapaneseName <$> rokuyo
  putStrLn $ fromMaybe "" rokuyoString  -- 大安

  let term = solarTerm jst $ fromGregorian 2000 3 20 :: Maybe SolarTerm
  let termString = solarTermToJapaneseName <$> term
  putStrLn $ fromMaybe "" termString  -- 春分

  let holiday = holidayType $ fromGregorian 2000 11 3 :: Maybe HolidayType
  let holidayString = holidayName <$> holiday
  putStrLn $ fromMaybe "" holidayString  -- 文化の日

formatTempoDate :: TempoDate -> String
formatTempoDate (TempoDate year month day) = show year ++ "年 " ++ tempoMonthToJapaneseName month ++ " " ++ show day ++ "日"
```
