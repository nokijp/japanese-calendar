module Data.Time.JapaneseCalendar.Rokuyo
  ( Rokuyo(..)
  , rokuyoToJapaneseName
  , rokuyoFromJapaneseName
  , rokuyoFromTempoDate
  , rokuyoFromGregorian
  ) where

import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Internal.DataUtils
import Data.Time.JapaneseCalendar.TempoCalendar
import Data.Time.LocalTime

data Rokuyo =
    Sensho  -- ^ 先勝
  | Tomobiki  -- ^ 友引
  | Senbu  -- ^ 先負
  | Butsumetsu  -- ^ 仏滅
  | Taian  -- ^ 大安
  | Shakko  -- ^ 赤口
    deriving (Show, Eq, Enum, Bounded)

japaneseNames :: [String]
japaneseNames =
  [ "先勝"
  , "友引"
  , "先負"
  , "仏滅"
  , "大安"
  , "赤口"
  ]

-- | returns a Japanese name of a rokuyo
rokuyoToJapaneseName :: Rokuyo -> String
rokuyoToJapaneseName = enumToName japaneseNames

-- | converts a Japanese name of a rokuyo into a SolarTerm
rokuyoFromJapaneseName :: String -> Maybe Rokuyo
rokuyoFromJapaneseName = enumFromName japaneseNames

-- | calculates a rokuyo from a Tempo date
rokuyoFromTempoDate :: TempoDate -> Rokuyo
rokuyoFromTempoDate (TempoDate _ month day) = toEnum $ (fromEnum (tempoMonthType month) + day - 1) `mod` 6

-- | calculates a rokuyo from a Gregorian date
rokuyoFromGregorian :: TimeZone -> Day -> Maybe Rokuyo
rokuyoFromGregorian zone day = rokuyoFromTempoDate <$> tempoDateFromGregorian zone day
