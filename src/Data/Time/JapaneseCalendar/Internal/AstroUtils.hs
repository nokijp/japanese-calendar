module Data.Time.JapaneseCalendar.Internal.AstroUtils
  ( julianCenturyFromUTCTime
  , utcTimeFromJulianCentury
  , solveSawtooth
  ) where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock

-- | converts a UTC time a Julian century.
julianCenturyFromUTCTime :: UTCTime -> Double
julianCenturyFromUTCTime t = diffDaysDouble t j2000 / 36525
  where
    diffDaysDouble (UTCTime d1 dt1) (UTCTime d2 dt2) = fromIntegral (diffDays d1 d2) + daysFromDiffTime (dt1 - dt2)

-- | converts a Julian century a UTC time.
utcTimeFromJulianCentury :: Double -> UTCTime
utcTimeFromJulianCentury jc = UTCTime (addDays days j2000Day) diffTime
  where
    UTCTime j2000Day j2000DayTime = j2000
    (days, diff) = (jc * 36525 + daysFromDiffTime j2000DayTime) `divMod'` 1
    diffTime = picosecondsToDiffTime $ round (diff * picosecondsInDay)

j2000 :: UTCTime
j2000 = UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime $ 12 * 60 * 60)

daysFromDiffTime :: DiffTime -> Double
daysFromDiffTime diff = fromIntegral (diffTimeToPicoseconds diff) / picosecondsInDay

picosecondsInDay :: Double
picosecondsInDay = 24 * 60 * 60 * 1e12

-- | solves an equation @f x = y@, in case @f@ has a similar form to a sawtooth.
solveSawtooth
  :: Double  -- ^ the amplitude of @f@
  -> Double  -- ^ the approximated cycle of @f@
  -> (Double -> Double)  -- ^ the function @f@, which has a similar form to a sawtooth
  -> Int  -- ^ the max iteration number
  -> Double  -- ^ the tolerance of @x@
  -> Double  -- ^ the initial guess of @x@
  -> Double  -- ^ the right hand side value @y@ of the equation
  -> Double  -- ^ the solution of the equation
solveSawtooth _ _ _ 0 _ initialX _ = initialX
solveSawtooth amplitude approxCycle f maxIterationNum epsX initialX finalY = finalX
  where
    finalX = if abs approxDiffX < epsX then initialX else solveSawtooth amplitude approxCycle f (maxIterationNum - 1) epsX newInitialX finalY
    initialY = f initialX
    diffRawY = finalY - initialY
    diffY = (diffRawY + amplitude / 2) `mod'` amplitude - amplitude / 2
    approxDiffX = diffY / (amplitude / approxCycle)
    newInitialX = initialX + approxDiffX
