module Data.Time.JapaneseCalendar.Internal.Moon
  ( moonEclipticLongitude
  , moonPhase
  , nearestNewMoon
  , averageMoonPhaseCycle
  ) where

import Data.Fixed
import Data.Time.Clock
import Data.Time.JapaneseCalendar.Internal.AstroUtils
import Data.Time.JapaneseCalendar.Internal.JulianCentury
import Data.Time.JapaneseCalendar.Internal.Sun

-- | calculates ecliptic longitude of the moon in degrees from a UTC time.
moonEclipticLongitude :: UTCTime -> Double
moonEclipticLongitude t = (4812.67881 * jc * 100 + 218.3161 + perturbationTerm1 + perturbationTerm2) `mod'` 360
  where
    jc = julianCenturyFromUTCTime t
    perturbationTerm1 = 6.2887 * sin ((4771.9886 * jc * 100 + 134.961 + perturbationSubTerm) * pi / 180)
    perturbationSubTerm = sum $ (\(p, q, r) -> p * sin ((q * jc * 100 + r) * pi / 180)) <$> perturbationCoefficients1
    perturbationTerm2 = sum $ (\(p, q, r) -> p * sin ((q * jc * 100 + r) * pi / 180)) <$> perturbationCoefficients2

-- | calculates the moon phase in degrees from a UTC time.
moonPhase :: UTCTime -> Double
moonPhase t = (moonEclipticLongitude t - sunEclipticLongitude t) `mod'` 360

-- | estimates the nearest day of new moon.
nearestNewMoon :: UTCTime -> UTCTime
nearestNewMoon initialTime = solution
  where
    solution = utcTimeFromJulianCentury finalJC
    finalJC = solveSawtooth 360 (averageMoonPhaseCycle / 36525) f 100 1e-10 (julianCenturyFromUTCTime initialTime) 0
    f = moonPhase . utcTimeFromJulianCentury

-- | the average cycle of the moon phase.
averageMoonPhaseCycle :: Fractional a => a
averageMoonPhaseCycle = 29.5

perturbationCoefficients1 :: [(Double, Double, Double)]
perturbationCoefficients1 =
  [ (0.004,   1.33, 119.5)
  , (0.002,  19.34,  55.0)
  , (0.0006,  0.2,   71.0)
  , (0.0006, 19.3,   54.0)
  ]

perturbationCoefficients2 :: [(Double, Double, Double)]
perturbationCoefficients2 =
  [ (1.274,   4133.3536, 100.738)
  , (0.6583,  8905.3422, 235.7)
  , (0.2136,  9543.9773, 269.926)
  , (0.1856,   359.9905, 177.525)
  , (0.1143,  9664.0404,   6.546)
  , (0.0588,   638.635,  214.22)
  , (0.0572,  3773.363,  103.21)
  , (0.0533, 13677.331,   10.66)
  , (0.0459,  8545.352,  238.18)
  , (0.041,   4411.998,  137.43)
  , (0.0348,  4452.671,  117.84)
  , (0.0305,  5131.979,  312.49)
  , (0.0153,   758.698,  130.84)
  , (0.0125, 14436.029,  141.51)
  , (0.011,   4892.052,  231.59)
  , (0.0107, 13038.696,  336.44)
  , (0.01,   14315.966,   44.89)
  , (0.0085,  8266.71,   201.5)
  , (0.0079,  4493.34,   278.2)
  , (0.0068,  9265.33,    53.2)
  , (0.0052,   319.32,   197.2)
  , (0.005,   4812.66,   295.4)
  , (0.0048,    19.34,   235.0)
  , (0.004,  13317.34,    13.2)
  , (0.004,  18449.32,   145.6)
  , (0.004,      1.33,   119.5)
  , (0.0039, 17810.68,   111.3)
  , (0.0037,  5410.62,   349.1)
  , (0.0027,  9183.99,   272.5)
  , (0.0026, 13797.39,   107.2)
  , (0.0024,   998.63,   211.9)
  , (0.0024,  9224.66,   252.8)
  , (0.0022,  8185.36,   240.6)
  , (0.0021,  9903.97,    87.5)
  , (0.0021,   719.98,   175.1)
  , (0.0021,  3413.37,   105.6)
  , (0.002,     19.34,    55.0)
  , (0.0018,  4013.29,     4.1)
  , (0.0016, 18569.38,   242.2)
  , (0.0012, 12678.71,   339.0)
  , (0.0011, 19208.02,   276.5)
  , (0.0009,  8586.0,    218.0)
  , (0.0008, 14037.3,    188.0)
  , (0.0008,  7906.7,    204.0)
  , (0.0007,  4052.0,    140.0)
  , (0.0007,  4853.3,    275.0)
  , (0.0007,   278.6,    216.0)
  , (0.0006,  1118.7,    128.0)
  , (0.0005, 22582.7,    247.0)
  , (0.0005, 19088.0,    181.0)
  , (0.0005, 17450.7,    114.0)
  , (0.0005,  5091.3,    332.0)
  , (0.0004,   398.7,    313.0)
  , (0.0004,   120.1,    278.0)
  , (0.0004,  9584.7,     71.0)
  , (0.0004,   720.0,     20.0)
  , (0.0003,  3814.0,     83.0)
  , (0.0003,  3494.7,     66.0)
  , (0.0003, 18089.3,    147.0)
  , (0.0003,  5492.0,    311.0)
  , (0.0003,    40.7,    161.0)
  , (0.0003, 23221.3,    280.0)
  ]
