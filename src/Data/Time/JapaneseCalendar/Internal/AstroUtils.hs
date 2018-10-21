module Data.Time.JapaneseCalendar.Internal.AstroUtils
  ( solveSawtooth
  ) where

import Data.Fixed

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
