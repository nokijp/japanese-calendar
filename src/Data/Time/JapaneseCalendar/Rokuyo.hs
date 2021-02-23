{-# LANGUAGE TemplateHaskell #-}

module Data.Time.JapaneseCalendar.Rokuyo
  ( Rokuyo(..)
  , rokuyoFromTempoDate
  , rokuyo
  ) where

import Data.Time.Calendar
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.TempoCalendar
import Data.Time.LocalTime

-- | a rokuyo
data Rokuyo =
    Sensho  -- ^ 先勝
  | Tomobiki  -- ^ 友引
  | Senbu  -- ^ 先負
  | Butsumetsu  -- ^ 仏滅
  | Taian  -- ^ 大安
  | Shakko  -- ^ 赤口
    deriving (Show, Eq, Ord, Enum, Bounded)

deriveJapaneseName ''Rokuyo ["先勝", "友引", "先負", "仏滅", "大安", "赤口"]

-- | calculates a rokuyo from a Tempo date
rokuyoFromTempoDate :: TempoDate -> Rokuyo
rokuyoFromTempoDate (TempoDate _ month day) = toEnum $ (fromEnum (tempoMonthType month) + day - 1) `mod` 6

-- | calculates a rokuyo from a date
rokuyo :: TimeZone -> Day -> Maybe Rokuyo
rokuyo zone day = rokuyoFromTempoDate <$> tempoDate zone day
