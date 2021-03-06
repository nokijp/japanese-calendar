module Data.Time.JapaneseCalendar.TempoCalendarSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.TempoCalendar
import Data.Time.JapaneseCalendar.Utils
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

  describe "monthNumber" $ do
    forM_
      [ (CommonMonth Mutsuki, 1)
      , (CommonMonth Shiwasu, 12)
      , (LeapMonth Mutsuki, 1)
      , (LeapMonth Shiwasu, 12)
      ] $ \(month, number) ->
        it ("should convert a month " ++ show month ++ " into a number " ++ show number) $
          monthNumber month `shouldBe` number

  describe "isLeapMonth" $ do
    it "should return False when given a common month" $ isLeapMonth (CommonMonth Mutsuki) `shouldBe` False
    it "should return True when given a leap month" $ isLeapMonth (LeapMonth Mutsuki) `shouldBe` True

  describe "toJapaneseName" $ do
    forM_
      [ (CommonMonth Mutsuki, "睦月")
      , (CommonMonth Shiwasu, "師走")
      , (LeapMonth Mutsuki, "閏睦月")
      , (LeapMonth Shiwasu, "閏師走")
      ] $ \(month, name) ->
        it ("should convert a month " ++ show month ++ " into a name " ++ show name) $
          toJapaneseName month `shouldBe` name

  describe "fromJapaneseName" $ do
    forM_
      [ (CommonMonth Mutsuki, "睦月")
      , (CommonMonth Shiwasu, "師走")
      , (LeapMonth Mutsuki, "閏睦月")
      , (LeapMonth Shiwasu, "閏師走")
      ] $ \(month, name) ->
        it ("should convert a name " ++ show name ++ " into a month " ++ show month) $
          fromJapaneseName name `shouldBe` Just month
    forM_
      [ ("")
      , ("abc")
      , ("閏")
      , ("閏abc")
      ] $ \(name) ->
        it ("should return Nothing when given an invalid name " ++ show name) $
          (fromJapaneseName name :: Maybe TempoMonth) `shouldBe` Nothing

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

  describe "tempoDate" $ do
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
        it ("should convert a Gregorian date " ++ show gregorian ++ " into a Tempo date " ++ show tempo) $
          tempoDate jst gregorian `shouldBe` Just tempo
    forM_
      ([2000..2032] ++ [2035..2100]) $ \(year) ->
        it ("should be defined in " ++ show year) $
          tempoDate jst (fromGregorian year 5 1) `shouldSatisfy` isJust
    forM_
      [2033, 2034] $ \(year) ->
        it ("should be indefinable in " ++ show year) $
          tempoDate jst (fromGregorian year 5 1) `shouldBe` Nothing

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
