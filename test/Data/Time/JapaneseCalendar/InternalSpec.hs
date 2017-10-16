module Data.Time.JapaneseCalendar.InternalSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Internal
import Prelude hiding (cycle)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sunEclipticLongitudeToTime" $ do
    forM_
      [ (0, timeFromYMDHMS 2000 3 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)
      , (0, timeFromYMDHMS 2000 4 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)
      , (90, timeFromYMDHMS 2000 6 1 0 0 0, timeFromYMDHMS 2000 6 21 1 48 0)
      , (90, timeFromYMDHMS 2000 7 1 0 0 0, timeFromYMDHMS 2000 6 21 1 48 0)
      , (180, timeFromYMDHMS 2000 9 1 0 0 0, timeFromYMDHMS 2000 9 22 17 28 0)
      , (180, timeFromYMDHMS 2000 10 1 0 0 0, timeFromYMDHMS 2000 9 22 17 28 0)
      , (270, timeFromYMDHMS 2000 12 1 0 0 0, timeFromYMDHMS 2000 12 21 13 37 0)
      , (270, timeFromYMDHMS 2001 1 1 0 0 0, timeFromYMDHMS 2000 12 21 13 37 0)
      , (360, timeFromYMDHMS 2000 3 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)
      , (360, timeFromYMDHMS 2000 4 1 0 0 0, timeFromYMDHMS 2000 3 20 7 35 0)

      , (0, timeFromYMDHMS 2010 3 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      , (0, timeFromYMDHMS 2010 4 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      , (90, timeFromYMDHMS 2010 6 1 0 0 0, timeFromYMDHMS 2010 6 21 11 28 0)
      , (90, timeFromYMDHMS 2010 7 1 0 0 0, timeFromYMDHMS 2010 6 21 11 28 0)
      , (180, timeFromYMDHMS 2010 9 1 0 0 0, timeFromYMDHMS 2010 9 23 3 9 0)
      , (180, timeFromYMDHMS 2010 10 1 0 0 0, timeFromYMDHMS 2010 9 23 3 9 0)
      , (270, timeFromYMDHMS 2010 12 1 0 0 0, timeFromYMDHMS 2010 12 21 23 28 0)
      , (270, timeFromYMDHMS 2011 1 1 0 0 0, timeFromYMDHMS 2010 12 21 23 28 0)
      , (360, timeFromYMDHMS 2010 3 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      , (360, timeFromYMDHMS 2010 4 1 0 0 0, timeFromYMDHMS 2010 3 20 17 32 0)
      ] $ \(longitude, inputTime, expectedTime) ->
        it ("should return " ++ show expectedTime ++ " when given " ++ show (longitude, inputTime)) $
          sunEclipticLongitudeToTime longitude inputTime `shouldSatisfy` (isAlmostEqual (20 * 60) expectedTime)

  describe "nearestNewMoon" $ do
    forM_
      [ (timeFromYMDHMS 2000 1 1 0 0 0, timeFromYMDHMS 2000 1 6 18 14 0)
      , (timeFromYMDHMS 2000 1 10 0 0 0, timeFromYMDHMS 2000 1 6 18 14 0)
      , (timeFromYMDHMS 2000 2 1 0 0 0, timeFromYMDHMS 2000 2 5 13 3 0)
      , (timeFromYMDHMS 2000 2 10 0 0 0, timeFromYMDHMS 2000 2 5 13 3 0)

      , (timeFromYMDHMS 2010 1 10 0 0 0, timeFromYMDHMS 2010 1 15 7 11 0)
      , (timeFromYMDHMS 2010 1 20 0 0 0, timeFromYMDHMS 2010 1 15 7 11 0)
      , (timeFromYMDHMS 2010 2 10 0 0 0, timeFromYMDHMS 2010 2 14 2 51 0)
      , (timeFromYMDHMS 2010 2 20 0 0 0, timeFromYMDHMS 2010 2 14 2 51 0)
      ] $ \(inputTime, expectedTime) ->
        it ("should return " ++ show expectedTime ++ " when given " ++ show inputTime) $
          nearestNewMoon inputTime `shouldSatisfy` (isAlmostEqual (20 * 60) expectedTime)

  describe "solveSawtooth" $ do
    forM_
      [ (10, 100, sawtooth 10 100 0, 10, 0.1, 30, 5, 50)
      , (10, 100, sawtooth 10 100 0, 10, 0.1, 70, 5, 50)
      , (10, 100, sawtooth 10 100 0, 10, 0.1, -20, 0, 0)
      , (10, 100, sawtooth 10 100 0, 10, 0.1, 20, 0, 0)
      , (10, 100, sawtooth 10 100 0, 10, 0.1, 80, 10, 100)
      , (10, 100, sawtooth 10 100 0, 10, 0.1, 120, 10, 100)

      , (10, 100, sawtooth 10 100 50, 10, 0.1, 80, 5, 100)
      , (10, 100, sawtooth 10 100 50, 10, 0.1, 120, 5, 100)
      , (10, 100, sawtooth 10 100 50, 10, 0.1, 30, 0, 50)
      , (10, 100, sawtooth 10 100 50, 10, 0.1, 70, 0, 50)
      , (10, 100, sawtooth 10 100 50, 10, 0.1, 130, 10, 150)
      , (10, 100, sawtooth 10 100 50, 10, 0.1, 170, 10, 150)

      , (10, 95, sawtooth 10 100 0, 10, 0.1, 30, 5, 50)
      , (10, 95, sawtooth 10 100 0, 10, 0.1, 70, 5, 50)
      , (10, 95, sawtooth 10 100 0, 10, 0.1, -20, 0, 0)
      , (10, 95, sawtooth 10 100 0, 10, 0.1, 20, 0, 0)
      , (10, 95, sawtooth 10 100 0, 10, 0.1, 80, 10, 100)
      , (10, 95, sawtooth 10 100 0, 10, 0.1, 120, 10, 100)

      , (10, 105, sawtooth 10 100 0, 10, 0.1, 30, 5, 50)
      , (10, 105, sawtooth 10 100 0, 10, 0.1, 70, 5, 50)
      , (10, 105, sawtooth 10 100 0, 10, 0.1, -20, 0, 0)
      , (10, 105, sawtooth 10 100 0, 10, 0.1, 20, 0, 0)
      , (10, 105, sawtooth 10 100 0, 10, 0.1, 80, 10, 100)
      , (10, 105, sawtooth 10 100 0, 10, 0.1, 120, 10, 100)

      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 30, 5, 50)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 70, 5, 50)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 0.1, 10, 0.1, -20, 0, 0)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 20, 0, 0)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 80, 10, 100)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 120, 10, 100)

      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 30, 5, 50)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 70, 5, 50)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, -20, 0, 0)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 20, 0, 0)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 80, 10, 100)
      , (10, 100, sawtooth 10 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 120, 10, 100)
      ] $ \(amplitude, approxCycle, f, maxIterationNum, epsX, initialX, finalY, solution) ->
        it ("should return " ++ show solution ++ " when given " ++ show (amplitude, approxCycle, maxIterationNum, epsX, initialX, finalY)) $
          solveSawtooth amplitude approxCycle f maxIterationNum epsX initialX finalY `shouldSatisfy` (\x -> abs (x - solution) < epsX)

  describe "solveCosMax" $ do
    forM_
      [ (100, coswave 100 0, 10, 0.1, 80, 100)
      , (100, coswave 100 0, 10, 0.1, 120, 100)

      , (100, coswave 100 50, 10, 0.1, 130, 150)
      , (100, coswave 100 50, 10, 0.1, 170, 150)

      , (95, coswave 100 0, 10, 0.1, 80, 100)
      , (95, coswave 100 0, 10, 0.1, 120, 100)

      , (105, coswave 100 0, 10, 0.1, 80, 100)
      , (105, coswave 100 0, 10, 0.1, 120, 100)

      , (100, coswave 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 80, 100)
      , (100, coswave 100 0 . phaseNoise 100 0 0.1, 10, 0.1, 120, 100)

      , (100, coswave 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 80, 100)
      , (100, coswave 100 0 . phaseNoise 100 0 (-0.1), 10, 0.1, 120, 100)
      ] $ \(approxCycle, f, maxIterationNum, epsX, initialX, solution) ->
        it ("should return " ++ show solution ++ " when given " ++ show (approxCycle, maxIterationNum, epsX, initialX)) $ do
          solveCosMax approxCycle f maxIterationNum epsX initialX `shouldSatisfy` (\x -> abs (x - solution) < epsX)

timeFromYMDHMS :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
timeFromYMDHMS year month day hour minute second = UTCTime (fromGregorian year month day) (fromIntegral $ hour * 60 * 60 + minute * 60 + second)

isAlmostEqual :: Integer -> UTCTime -> UTCTime -> Bool
isAlmostEqual toleranceInSeconds a b = abs (diffUTCTime a b) < fromIntegral toleranceInSeconds

sawtooth :: Double -> Double -> Double -> Double -> Double
sawtooth amplitude cycle offset x = amplitude * cyclize ((x - offset) / cycle)
  where
    cyclize x' = x' - fromIntegral (floor x' :: Int)

coswave :: Double -> Double -> Double -> Double
coswave cycle offset x = cos ((x - offset) / cycle * 2 * pi)

phaseNoise :: Double -> Double -> Double -> Double -> Double
phaseNoise cycle offset amount x = x + amount * sin ((x - offset) / cycle)
