{-# LANGUAGE TemplateHaskell #-}

module Data.Time.JapaneseCalendar.SolarTerm
  ( SolarTerm(..)
  , isSegmentPoint
  , isCenterPoint
  , solarTerm
  , latestSolarTerm
  , nearestSolarTerm
  , solarTermsFrom
  , findNearestSolarTerm
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.JapaneseCalendar.Internal.Sun
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.LocalTime

-- | the twenty-four solar terms
data SolarTerm =
    VernalEquinox  -- ^ 春分
  | ClearAndBright  -- ^ 清明
  | GrainRain  -- ^ 穀雨
  | StartOfSummer  -- ^ 立夏
  | GrainFull  -- ^ 小満
  | GrainInEar  -- ^ 芒種
  | SummerSolstice  -- ^ 夏至
  | MinorHeat  -- ^ 小暑
  | MajorHeat  -- ^ 大暑
  | StartOfAutumn  -- ^ 立秋
  | LimitOfHeat  -- ^ 処暑
  | WhiteDew  -- ^ 白露
  | AutumnalEquinox  -- ^ 秋分
  | ColdDew  -- ^ 寒露
  | FrostDescent  -- ^ 霜降
  | StartOfWinter  -- ^ 立冬
  | MinorSnow  -- ^ 小雪
  | MajorSnow  -- ^ 大雪
  | WinterSolstice  -- ^ 冬至
  | MinorCold  -- ^ 小寒
  | MajorCold  -- ^ 大寒
  | StartOfSpring  -- ^ 立春
  | RainWater  -- ^ 雨水
  | AwakeningOfInsects  -- ^ 啓蟄
    deriving (Show, Eq, Ord, Bounded, Enum)

deriveJapaneseName ''SolarTerm
  [ "春分"
  , "清明"
  , "穀雨"
  , "立夏"
  , "小満"
  , "芒種"
  , "夏至"
  , "小暑"
  , "大暑"
  , "立秋"
  , "処暑"
  , "白露"
  , "秋分"
  , "寒露"
  , "霜降"
  , "立冬"
  , "小雪"
  , "大雪"
  , "冬至"
  , "小寒"
  , "大寒"
  , "立春"
  , "雨水"
  , "啓蟄"
  ]

-- | tests whether the solar term is a segment point, sekki
isSegmentPoint :: SolarTerm -> Bool
isSegmentPoint term = fromEnum term `mod` 2 == 1

-- | tests whether the solar term is a center point, chuki
isCenterPoint :: SolarTerm -> Bool
isCenterPoint = not . isSegmentPoint

-- | returns the solar term of the specified day
solarTerm :: TimeZone -> Day -> Maybe SolarTerm
solarTerm zone day = if startTerm /= endTerm then Just endTerm else Nothing
  where
    startTerm = latestSolarTermUTC startOfDay
    endTerm = latestSolarTermUTC endOfDay
    startOfDay = localTimeToUTC zone (LocalTime day midnight)
    endOfDay = localTimeToUTC zone (LocalTime (succ day) midnight)

-- | finds the latest solar term
latestSolarTerm :: TimeZone -> Day -> (SolarTerm, Day)
latestSolarTerm zone day = (term, findNearestSolarTerm zone term day)
  where
    term = latestSolarTermUTC utcTime
    utcTime = localTimeToUTC zone (LocalTime (succ day) midnight)

-- | finds the nearest solar term
nearestSolarTerm :: TimeZone -> Day -> (SolarTerm, Day)
nearestSolarTerm zone day = (term, findNearestSolarTerm zone term day)
  where
    term = toEnum index
    index = round (sunEclipticLongitude utcTime / 15) `mod` 24
    utcTime = localTimeToUTC zone (LocalTime day midday)

-- | creates a list of solar terms starting with the nearest solar term from the specified day
solarTermsFrom :: TimeZone -> Day -> [(SolarTerm, Day)]
solarTermsFrom zone day = scanl (\(_, d) term -> (term, findNearestSolarTerm zone term $ addDays 15 d)) firstElem restSolarTerms
  where
    firstElem@(firstSolarTerm, _) = nearestSolarTerm zone day
    firstIndex = fromEnum firstSolarTerm
    restSolarTerms = cycle $ toEnum . (`mod` 24) <$> take 24 [(firstIndex + 1)..]

-- | finds the nearest specified solar term
findNearestSolarTerm :: TimeZone -> SolarTerm -> Day -> Day
findNearestSolarTerm zone term day = divisionDay
  where
    utcTime = localTimeToUTC zone (LocalTime day midday)
    divisionUTCTime = sunEclipticLongitudeToTime (fromIntegral $ fromEnum term * 15) utcTime
    divisionTime = utcToZonedTime zone divisionUTCTime
    divisionDay = localDay $ zonedTimeToLocalTime divisionTime

latestSolarTermUTC :: UTCTime -> SolarTerm
latestSolarTermUTC utcTime = term
  where
    term = toEnum index
    index = floor (sunEclipticLongitude utcTime / 15) `mod` 24
