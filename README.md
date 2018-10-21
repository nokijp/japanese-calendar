# Japanese Calendar

[![Build Status](https://travis-ci.org/nokijp/japanese-calendar.svg?branch=master)](https://travis-ci.org/nokijp/japanese-calendar)

This is a library for Haskell that provides functions to deal with the Japanese calendar.

[The haddocks are available here.](https://nokijp.github.io/japanese-calendar/)

This library contains:

- Tempo calendar (天保暦)
- Rokuyo (六曜)
- the twenty-four solar terms (二十四節気)
- the stems and branches (干支)
- Japanese public holidays (国民の祝日)

```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Data.Maybe
import Data.Time.Calendar
import Data.Time.JapaneseCalendar

main :: IO ()
main = do
  let tempoDate20010523 = tempoDate jst $ fromGregorian 2001 5 23 :: Maybe TempoDate
  putStrLn $ format $ fromJust tempoDate20010523  -- 2001年 閏4月 1日

  let rokuyo20000614 = rokuyo jst $ fromGregorian 2000 6 14 :: Maybe Rokuyo
  putStrLn $ toJapaneseName $ fromJust rokuyo20000614  -- 大安

  let solarTerm20000320 = solarTerm jst $ fromGregorian 2000 3 20 :: Maybe SolarTerm
  putStrLn $ toJapaneseName $ fromJust solarTerm20000320  -- 春分

  let stemBranch20000101 = dayStemBranch $ fromGregorian 2000 1 1 :: StemBranch
  putStrLn $ toJapaneseName stemBranch20000101  -- 戊午

  let holiday20001103 = holidayType $ fromGregorian 2000 11 3 :: Maybe HolidayType
  putStrLn $ toJapaneseName $ fromJust holiday20001103  -- 文化の日

format :: TempoDate -> String
format = [qFormatTempoDate|%y年 %M月 %d日|]
```
