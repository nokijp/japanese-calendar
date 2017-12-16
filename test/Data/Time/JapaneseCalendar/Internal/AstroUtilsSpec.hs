module Data.Time.JapaneseCalendar.Internal.AstroUtilsSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.JapaneseCalendar.Internal.AstroUtils
import Prelude hiding (cycle)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

sawtooth :: Double -> Double -> Double -> Double -> Double
sawtooth amplitude cycle offset x = amplitude * cyclize ((x - offset) / cycle)
  where
    cyclize x' = x' - fromIntegral (floor x' :: Int)

phaseNoise :: Double -> Double -> Double -> Double -> Double
phaseNoise cycle offset amount x = x + amount * sin ((x - offset) / cycle)
