{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.Time.JapaneseCalendar.FormatSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.JapaneseCalendar.Format
import Data.Time.JapaneseCalendar.TempoCalendar
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatTempoDate" $ do
    forM_
      [ (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate||], "")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%y年%M月%d日|], "2000年1月2日")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%y年%M月%d日|], "2000年閏1月2日")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%04y/%02M/%02d|], "2000/閏01/02")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%y,%3y,%4y,%5y,%03y,%04y,%05y|], "2000,2000,2000, 2000,2000,2000,02000")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%L|], "")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%L|], "閏")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%m,%1m,%2m,%3m,%01m,%02m,%03m|], "1,1, 1,  1,1,01,001")
      , (TempoDate 2000 (CommonMonth Shiwasu) 2, [formatTempoDate|%m,%1m,%2m,%3m,%01m,%02m,%03m|], "12,12,12, 12,12,12,012")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%m,%1m,%2m,%3m,%01m,%02m,%03m|], "1,1, 1,  1,1,01,001")
      , (TempoDate 2000 (LeapMonth Shiwasu) 2, [formatTempoDate|%m,%1m,%2m,%3m,%01m,%02m,%03m|], "12,12,12, 12,12,12,012")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%M,%1M,%2M,%3M,%01M,%02M,%03M|], "1,1, 1,  1,1,01,001")
      , (TempoDate 2000 (CommonMonth Shiwasu) 2, [formatTempoDate|%M,%1M,%2M,%3M,%01M,%02M,%03M|], "12,12,12, 12,12,12,012")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%M,%1M,%2M,%3M,%01M,%02M,%03M|], "閏1,閏1,閏 1,閏  1,閏1,閏01,閏001")
      , (TempoDate 2000 (LeapMonth Shiwasu) 2, [formatTempoDate|%M,%1M,%2M,%3M,%01M,%02M,%03M|], "閏12,閏12,閏12,閏 12,閏12,閏12,閏012")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%b|], "睦月")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%b|], "睦月")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%B|], "睦月")
      , (TempoDate 2000 (LeapMonth Mutsuki) 2, [formatTempoDate|%B|], "閏睦月")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%d,%1d,%2d,%3d,%01d,%02d,%03d|], "2,2, 2,  2,2,02,002")
      , (TempoDate 2000 (CommonMonth Mutsuki) 23, [formatTempoDate|%d,%1d,%2d,%3d,%01d,%02d,%03d|], "23,23,23, 23,23,23,023")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%%|], "%")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%%%%|], "%%")
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, [formatTempoDate|%%%d|], "%2")
      ] $ \(day, formatter, expected) ->
        it ("should return " ++ show expected ++ " when given " ++ show day) $
          formatter day `shouldBe` expected

    forM_
      [ "%1L"
      , "%01L"
      , "%1b"
      , "%01b"
      , "%1B"
      , "%01B"
      , "%"
      , "%00d"
      , "%x"
      ] $ \format ->
        it ("should fail when given invalid format " ++ show format) $
          (runQ $ quoteExp formatTempoDate format) `shouldThrow` anyException
