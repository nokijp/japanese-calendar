module Data.Time.JapaneseCalendar.TempoCalendarSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.TempoCalendar
import Test.Hspec
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TempoMonth" $ do
    it "should be ordered correctly" $ forAll (shuffle sortedTempoMonths) $ \months ->
      sort months == sortedTempoMonths

  describe "TempoDate" $ do
    it "should be ordered correctly" $ forAll (shuffle sortedTempoDates) $ \dates ->
      sort dates == sortedTempoDates

  describe "tempoMonthToJapaneseName" $ do
    forM_
      [ (CommonMonth Mutsuki, "睦月")
      , (CommonMonth Shiwasu, "師走")
      , (LeapMonth Mutsuki, "閏睦月")
      , (LeapMonth Shiwasu, "閏師走")
      ] $ \(month, name) ->
        it ("should convert the month " ++ show month ++ " into the name " ++ show name) $
          tempoMonthToJapaneseName month `shouldBe` name

  describe "tempoMonthFromJapaneseName" $ do
    forM_
      [ (CommonMonth Mutsuki, "睦月")
      , (CommonMonth Shiwasu, "師走")
      , (LeapMonth Mutsuki, "閏睦月")
      , (LeapMonth Shiwasu, "閏師走")
      ] $ \(month, name) ->
        it ("should convert the name " ++ show name ++ " into the month " ++ show month) $
          tempoMonthFromJapaneseName name `shouldBe` Just month
    forM_
      [ ("")
      , ("abc")
      , ("閏")
      , ("閏abc")
      ] $ \(name) ->
        it ("should return Nothing when given the invalid name " ++ show name) $
          tempoMonthFromJapaneseName name `shouldBe` Nothing

  describe "nextTempoMonthType" $ do
    forM_
      [ (Mutsuki, Kisaragi)
      , (Shiwasu, Mutsuki)
      ] $ \(current, next) ->
        it ("should return " ++ show current ++ " when given " ++ show next) $
          nextTempoMonthType current `shouldBe` next

  describe "previousTempoMonthType" $ do
    forM_
      [ (Mutsuki, Shiwasu)
      , (Shiwasu, Shimotsuki)
      ] $ \(current, prev) ->
        it ("should return " ++ show current ++ " when given " ++ show prev) $
          previousTempoMonthType current `shouldBe` prev

  describe "tempoDateFromGregorian" $ do
    forM_
      [ (fromGregorian 2000 1 1, TempoDate 1999 (CommonMonth Shimotsuki) 25)
      , (fromGregorian 2000 1 2, TempoDate 1999 (CommonMonth Shimotsuki) 26)
      , (fromGregorian 2000 1 6, TempoDate 1999 (CommonMonth Shimotsuki) 30)
      , (fromGregorian 2000 1 7, TempoDate 1999 (CommonMonth Shiwasu) 1)
      , (fromGregorian 2000 2 4, TempoDate 1999 (CommonMonth Shiwasu) 29)
      , (fromGregorian 2000 2 5, TempoDate 2000 (CommonMonth Mutsuki) 1)
      , (fromGregorian 2001 4 24, TempoDate 2001 (CommonMonth Uzuki) 1)
      , (fromGregorian 2001 5 23, TempoDate 2001 (LeapMonth Uzuki) 1)
      , (fromGregorian 2001 6 21, TempoDate 2001 (CommonMonth Satsuki) 1)
      , (fromGregorian 2010 1 1, TempoDate 2009 (CommonMonth Shimotsuki) 17)
      ] $ \(gregorian, tempo) ->
        it ("should convert the Gregorian date " ++ show gregorian ++ " into the Tempo date " ++ show tempo) $
          tempoDateFromGregorian jst gregorian `shouldBe` Just tempo
    forM_
      ([2000..2032] ++ [2035..2100]) $ \(year) ->
        it ("should be defined in " ++ show year) $
          tempoDateFromGregorian jst (fromGregorian year 5 1) `shouldSatisfy` isJust
    forM_
      [2033, 2034] $ \(year) ->
        it ("should be indefinable in " ++ show year) $
          tempoDateFromGregorian jst (fromGregorian year 5 1) `shouldBe` Nothing

sortedTempoMonths :: [TempoMonth]
sortedTempoMonths =
  [ CommonMonth Mutsuki
  , LeapMonth Mutsuki
  , CommonMonth Kisaragi
  , LeapMonth Kisaragi
  , CommonMonth Yayoi
  , LeapMonth Yayoi
  , CommonMonth Uzuki
  , LeapMonth Uzuki
  , CommonMonth Satsuki
  , LeapMonth Satsuki
  , CommonMonth Minazuki
  , LeapMonth Minazuki
  , CommonMonth Fumizuki
  , LeapMonth Fumizuki
  , CommonMonth Hazuki
  , LeapMonth Hazuki
  , CommonMonth Nagatsuki
  , LeapMonth Nagatsuki
  , CommonMonth Kannazuki
  , LeapMonth Kannazuki
  , CommonMonth Shimotsuki
  , LeapMonth Shimotsuki
  , CommonMonth Shiwasu
  , LeapMonth Shiwasu
  ]

sortedTempoDates :: [TempoDate]
sortedTempoDates =
  [ TempoDate 1999 (CommonMonth Mutsuki) 1
  , TempoDate 1999 (CommonMonth Mutsuki) 2
  , TempoDate 1999 (CommonMonth Kisaragi) 1
  , TempoDate 1999 (CommonMonth Shiwasu) 1
  , TempoDate 2000 (CommonMonth Mutsuki) 1
  , TempoDate 2000 (CommonMonth Mutsuki) 2
  , TempoDate 2000 (CommonMonth Kisaragi) 1
  , TempoDate 2000 (CommonMonth Shiwasu) 1
  ]
