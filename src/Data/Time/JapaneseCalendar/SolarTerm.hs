module Data.Time.JapaneseCalendar.SolarTerm
  ( SolarTerm(..)
  , solarTermToJapaneseName
  , solarTermFromJapaneseName
  , isSegmentPoint
  , isCenterPoint
  , solarTerm
  , nearestSolarTerm
  , solarTermsFrom
  , findNearestSolarTerm
  ) where

import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Internal.Astronomy
import Data.Time.JapaneseCalendar.Internal.DataUtils
import Data.Time.LocalTime

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
    deriving (Show, Eq, Bounded, Enum)

japaneseNames :: [String]
japaneseNames =
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

-- | returns a Japanese name of a solar term
solarTermToJapaneseName :: SolarTerm -> String
solarTermToJapaneseName = enumToName japaneseNames

-- | converts a Japanese name of a solar term into a SolarTerm
solarTermFromJapaneseName :: String -> Maybe SolarTerm
solarTermFromJapaneseName = enumFromName japaneseNames

-- | tests whether the solar term is a segment point, sekki
isSegmentPoint :: SolarTerm -> Bool
isSegmentPoint term = fromEnum term `mod` 2 == 1

-- | tests whether the solar term is a center point, chuki
isCenterPoint :: SolarTerm -> Bool
isCenterPoint = not . isSegmentPoint

-- | returns a solar term of a specified day
solarTerm :: TimeZone -> Day -> Maybe SolarTerm
solarTerm zone day = if day == divisionDay then Just term else Nothing
  where
    (term, divisionDay) = nearestSolarTerm zone day

-- | finds the nearest solar term
nearestSolarTerm :: TimeZone -> Day -> (SolarTerm, Day)
nearestSolarTerm zone day = fromIndex zone index day
  where
    utcTime = localTimeToUTC zone (LocalTime day midday)
    index = round (sunEclipticLongitude utcTime / 15) `mod` 24

-- | creates a list of solar terms starting with the nearest solar term from a specified day
solarTermsFrom :: TimeZone -> Day -> [(SolarTerm, Day)]
solarTermsFrom zone day = firstElem : solarTermsFrom zone nextSeed
  where
    firstElem@(_, firstElemDay) = nearestSolarTerm zone day
    nextSeed = addDays 15 firstElemDay

-- | finds the nearest specified solar term
findNearestSolarTerm :: TimeZone -> SolarTerm -> Day -> Day
findNearestSolarTerm zone term day = snd $ fromIndex zone (fromEnum term) day

fromIndex :: TimeZone -> Int -> Day -> (SolarTerm, Day)
fromIndex zone index day = (toEnum index, divisionDay)
  where
    utcTime = localTimeToUTC zone (LocalTime day midday)
    divisionUTCTime = sunEclipticLongitudeToTime (fromIntegral $ index * 15) utcTime
    divisionTime = utcToZonedTime zone divisionUTCTime
    divisionDay = localDay $ zonedTimeToLocalTime divisionTime
