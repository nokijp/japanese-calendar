module Data.Time.JapaneseCalendar.Internal.JulianCentury
  ( julianCenturyFromUTCTime
  , utcTimeFromJulianCentury
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.TAI

-- | converts a UTC time to a Julian century in TDT.
julianCenturyFromUTCTime :: UTCTime -> Double
julianCenturyFromUTCTime utcTime =
  let
    taiTime = utcToTAI utcTime
    taiDiffSeconds = realToFrac $ diffAbsoluteTime taiTime j2000TAI
    jc = taiDiffSeconds / centuryInSeconds
  in jc

-- | converts a Julian century in TDT to a UTC time.
utcTimeFromJulianCentury :: Double -> UTCTime
utcTimeFromJulianCentury jc =
  let
    taiDiffSeconds = realToFrac $ jc * centuryInSeconds
    taiTime = addAbsoluteTime taiDiffSeconds j2000TAI
    utcTime = taiToUTC taiTime
  in utcTime

centuryInSeconds :: Double
centuryInSeconds = 36525 * 86400

j2000TAI :: AbsoluteTime
j2000TAI = utcToTAI $ UTCTime (fromGregorian 2000 1 1) (picosecondsToDiffTime $ (43200000 - 32184) * 10 ^ (9 :: Int))

utcToTAI :: UTCTime -> AbsoluteTime
utcToTAI = fromJust . utcToTAITime leapSecondMap1958

taiToUTC :: AbsoluteTime -> UTCTime
taiToUTC = fromJust . taiToUTCTime leapSecondMap1958

leapSecondMap1958 :: Day -> Maybe Int
leapSecondMap1958 utcDay =
  let
    tableValue = snd <$> M.lookupLE utcDay leapSecondTable1958
    leapSeconds = fromMaybe 0 tableValue
  in Just leapSeconds

leapSecondTable1958 :: Map Day Int
leapSecondTable1958 = M.fromList
  [ (fromGregorian 1958 1 1,  0)  -- pseudo
  , (fromGregorian 1959 1 1,  1)  -- pseudo
  , (fromGregorian 1961 1 1,  2)  -- pseudo
  , (fromGregorian 1963 1 1,  3)  -- pseudo
  , (fromGregorian 1964 7 1,  4)  -- pseudo
  , (fromGregorian 1966 1 1,  5)  -- pseudo
  , (fromGregorian 1967 7 1,  6)  -- pseudo
  , (fromGregorian 1969 1 1,  7)  -- pseudo
  , (fromGregorian 1970 1 1,  8)  -- pseudo
  , (fromGregorian 1971 1 1,  9)  -- pseudo
  , (fromGregorian 1972 1 1, 10)
  , (fromGregorian 1972 7 1, 11)
  , (fromGregorian 1973 1 1, 12)
  , (fromGregorian 1974 1 1, 13)
  , (fromGregorian 1975 1 1, 14)
  , (fromGregorian 1976 1 1, 15)
  , (fromGregorian 1977 1 1, 16)
  , (fromGregorian 1978 1 1, 17)
  , (fromGregorian 1979 1 1, 18)
  , (fromGregorian 1980 1 1, 19)
  , (fromGregorian 1981 7 1, 20)
  , (fromGregorian 1982 7 1, 21)
  , (fromGregorian 1983 7 1, 22)
  , (fromGregorian 1985 7 1, 23)
  , (fromGregorian 1988 1 1, 24)
  , (fromGregorian 1990 1 1, 25)
  , (fromGregorian 1991 1 1, 26)
  , (fromGregorian 1992 7 1, 27)
  , (fromGregorian 1993 7 1, 28)
  , (fromGregorian 1994 7 1, 29)
  , (fromGregorian 1996 1 1, 30)
  , (fromGregorian 1997 7 1, 31)
  , (fromGregorian 1999 1 1, 32)
  , (fromGregorian 2006 1 1, 33)
  , (fromGregorian 2009 1 1, 34)
  , (fromGregorian 2012 7 1, 35)
  , (fromGregorian 2015 7 1, 36)
  , (fromGregorian 2017 1 1, 37)
  ]
