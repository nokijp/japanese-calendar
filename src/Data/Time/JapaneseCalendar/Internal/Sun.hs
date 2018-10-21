module Data.Time.JapaneseCalendar.Internal.Sun
  ( sunEclipticLongitude
  , sunEclipticLongitudeToTime
  ) where

import Data.Fixed
import Data.Time.Clock
import Data.Time.JapaneseCalendar.Internal.AstroUtils
import Data.Time.JapaneseCalendar.Internal.JulianCentury

-- | calculates ecliptic longitude of the sun in degrees from a UTC time.
sunEclipticLongitude :: UTCTime -> Double
sunEclipticLongitude t = (36000.7695 * jc + 280.4602 + perturbationTerm) `mod'` 360
  where
    jc = julianCenturyFromUTCTime t
    perturbationTerm = sum $ (\(e, p, q, r) -> p * jc ^ e * cos ((q * jc + r) * pi / 180)) <$> perturbationCoefficients

-- | estimates the nearest day when the sun's ecliptic longitude is the given value.
sunEclipticLongitudeToTime :: Double -> UTCTime -> UTCTime
sunEclipticLongitudeToTime longitudeInDegrees initialTime = solution
  where
    solution = utcTimeFromJulianCentury finalJC
    finalJC = solveSawtooth 360 0.01 f 100 1e-10 (julianCenturyFromUTCTime initialTime) longitudeInDegrees
    f = sunEclipticLongitude . utcTimeFromJulianCentury

perturbationCoefficients :: [(Int, Double, Double, Double)]
perturbationCoefficients =
  [ (0,  1.9147,  35999.05, 267.52)
  , (0,  0.02,    71998.1,  265.1)
  , (0,  0.002,   32964.0,  158.0)
  , (0,  0.0018,     19.0,  159.0)
  , (0,  0.0018, 445267.0,  208.0)
  , (0,  0.0015,  45038.0,  254.0)
  , (0,  0.0013,  22519.0,  352.0)
  , (0,  0.0007,  65929.0,   45.0)
  , (0,  0.0007,   3035.0,  110.0)
  , (0,  0.0007,   9038.0,   64.0)
  , (0,  0.0006,  33718.0,  316.0)
  , (0,  0.0005,    155.0,  118.0)
  , (0,  0.0005,   2281.0,  221.0)
  , (0,  0.0004,  29930.0,   48.0)
  , (0,  0.0004,  31557.0,  161.0)
  , (1, -0.0048,  35999.0,  268.0)
  , (1,  0.0048,   1934.0,  145.0)
  , (1, -0.0004,  72002.0,  111.0)
  ]
