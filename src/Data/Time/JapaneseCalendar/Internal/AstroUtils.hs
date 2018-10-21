module Data.Time.JapaneseCalendar.Internal.AstroUtils
  ( julianCenturyFromUTCTime
  , utcTimeFromJulianCentury
  , solveSawtooth
  ) where

import Data.Fixed
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.TAI

-- | converts a UTC time to a Julian century in TDT.
julianCenturyFromUTCTime :: UTCTime -> Double
julianCenturyFromUTCTime utcTime =
  let
    taiTime = utcToTAI2000 utcTime
    taiDiffSeconds = realToFrac $ diffAbsoluteTime taiTime j2000TAI
    jc = taiDiffSeconds / centuryInSeconds
  in jc

-- | converts a Julian century in TDT to a UTC time.
utcTimeFromJulianCentury :: Double -> UTCTime
utcTimeFromJulianCentury jc =
  let
    taiDiffSeconds = realToFrac $ jc * centuryInSeconds
    taiTime = addAbsoluteTime taiDiffSeconds j2000TAI
    utcTime = taiToUTC2000 taiTime
  in utcTime

centuryInSeconds :: Double
centuryInSeconds = 36525 * 86400

j2000TAI :: AbsoluteTime
j2000TAI = utcToTAI2000 $ UTCTime (fromGregorian 2000 1 1) (picosecondsToDiffTime $ (43200000 - 32184) * 10 ^ (9 :: Int))

utcToTAI2000 :: UTCTime -> AbsoluteTime
utcToTAI2000 = fromJust . utcToTAITime (const (Just 32))

taiToUTC2000 :: AbsoluteTime -> UTCTime
taiToUTC2000 = fromJust . taiToUTCTime (const (Just 32))

-- | solves an equation @f x = y@, in case @f@ has a similar form to sawtooth.
solveSawtooth
  :: Double  -- ^ the amplitude of @f@
  -> Double  -- ^ the approximated cycle of @f@
  -> (Double -> Double)  -- ^ the function @f@, which has a similar form to sawtooth
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
