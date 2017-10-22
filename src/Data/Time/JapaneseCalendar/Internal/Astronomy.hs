module Data.Time.JapaneseCalendar.Internal.Astronomy
  ( sunEclipticLongitude
  , sunEclipticLongitudeToTime
  , nearestNewMoon
  , averageNewMoonCycle
  , solveSawtooth
  , solveCosMax
  ) where

import Data.Astro.Moon
import Data.Astro.Moon.MoonDetails
import Data.Astro.Sun
import Data.Astro.Time.JulianDate
import Data.Astro.Types
import Data.Fixed
import Data.Time.Clock
import Data.Time.Calendar

sunEclipticLongitude :: UTCTime -> Double
sunEclipticLongitude time = let (DD degree) = sunEclipticLongitude2 $ sunDetails $ utcTimeToJulianDate time in degree

-- | estimates the nearest day when the sun's ecliptic longitude is the given value.
sunEclipticLongitudeToTime :: Double -> UTCTime -> UTCTime
sunEclipticLongitudeToTime longitudeInDegrees initialTime = solution
  where
    solution = julianDateToUTCTime $ JD finalJd
    finalJd = solveSawtooth 360 365.25 f 100 1e-5 initialJd longitudeInDegrees
    (JD initialJd) = utcTimeToJulianDate initialTime
    f x = let (DD y) = sunEclipticLongitude2 (sunDetails (JD x)) in y

-- | estimates the nearest day of new moon.
nearestNewMoon :: UTCTime -> UTCTime
nearestNewMoon initialTime = solution
  where
    solution = julianDateToUTCTime $ JD finalJd
    finalJd = solveCosMax averageNewMoonCycle f 100 1e-5 initialJd
    (JD initialJd) = utcTimeToJulianDate initialTime
    f x = 1 - 2 * moonPhase j2010MoonDetails (JD x)

averageNewMoonCycle :: Fractional a => a
averageNewMoonCycle = 29.5

julianDateToUTCTime :: JulianDate -> UTCTime
julianDateToUTCTime jd = UTCTime dayPart timePart
  where
    (year, month, day, hour, minute, second) = toYMDHMS jd
    dayPart = fromGregorian year month day
    timePartInSeconds = fromIntegral (hour * 60 * 60 + minute * 60) + second
    timePartInPicoSeconds = round $ timePartInSeconds * 1e12
    timePart = picosecondsToDiffTime timePartInPicoSeconds

utcTimeToJulianDate :: UTCTime -> JulianDate
utcTimeToJulianDate (UTCTime dayPart timePart) = fromYMDHMS year month day hour minute second
  where
    (year, month, day) = toGregorian dayPart
    timePartInSeconds = fromIntegral (diffTimeToPicoseconds timePart) * 1e-12
    (hour, hourRemainder) = timePartInSeconds `divMod'` (60 * 60)
    (minute, second) = hourRemainder `divMod'` 60

-- | solves the equation @f x = y@, in case @f@ has a similar form to a sawtooth.
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

-- | solves the equation @f x = 1@, in case @f@ has a similar form to a cosine wave.
solveCosMax
  :: Double  -- ^ the approximated cycle of @f@
  -> (Double -> Double)  -- ^ the function @f@, which has a similar form to a cosine wave
  -> Int  -- ^ the max iteration number
  -> Double  -- ^ the tolerance of @x@
  -> Double  -- ^ the initial guess of @x@
  -> Double  -- ^ the solution of the equation
solveCosMax _ _ 0 _ initialX = initialX
solveCosMax approxCycle f maxIterationNum epsX initialX = finalX
  where
    finalX = if abs approxPhaseDiff < epsX then initialX else solveCosMax approxCycle f (maxIterationNum - 1) epsX newInitialX
    initialY = f initialX
    approxPhaseDiff = approxCycle * acos initialY / (2 * pi)
    plusX = initialX + approxPhaseDiff
    minusX = initialX - approxPhaseDiff
    newInitialX = if diff (f plusX) < diff (f minusX) then plusX else minusX
    diff y = abs $ y - 1
