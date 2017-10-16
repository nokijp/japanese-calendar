module Data.Time.JapaneseCalendar.SolarTerm
  ( SolarTerm(..)
  , solarTermToJapaneseName
  , solarTermFromJapaneseName
  , solarTerm
  ) where

import Data.List
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Internal
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

-- | returns a name of a solar term
solarTermToJapaneseName :: SolarTerm -> String
solarTermToJapaneseName term = japaneseNames !! fromEnum term

-- | converts a Japanese name of a solar term into a SolarTerm
solarTermFromJapaneseName :: String -> Maybe SolarTerm
solarTermFromJapaneseName name = toEnum <$> elemIndex name japaneseNames

-- | returns a solar term of a specified day
solarTerm :: TimeZone -> Day -> Maybe SolarTerm
solarTerm zone day = if day == getDay divisionTime then Just (toEnum nearestDivisionIndex) else Nothing
  where
    getDay = localDay . zonedTimeToLocalTime
    utcTime = localTimeToUTC zone (LocalTime day midday)
    nearestDivisionIndex = round (sunEclipticLongitude utcTime / 15) `mod` 24
    divisionUTCTime = sunEclipticLongitudeToTime (fromIntegral $ nearestDivisionIndex * 15) utcTime
    divisionTime = utcToZonedTime zone divisionUTCTime
