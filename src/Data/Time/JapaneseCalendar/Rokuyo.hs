module Data.Time.JapaneseCalendar.Rokuyo
  ( Rokuyo(..)
  , rokuyoFromTempoDate
  , rokuyoFromGregorian
  ) where

import Data.Time.Calendar
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

rokuyoFromTempoDate :: TempoDate -> Rokuyo
rokuyoFromTempoDate (TempoDate _ month day) = toEnum $ (fromEnum (tempoMonthType month) + day - 1) `mod` 6

rokuyoFromGregorian :: TimeZone -> Day -> Maybe Rokuyo
rokuyoFromGregorian zone day = rokuyoFromTempoDate <$> tempoDateFromGregorian zone day