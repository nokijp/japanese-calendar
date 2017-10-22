module Data.Time.JapaneseCalendar.TempoCalendarSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Maybe
import Data.Time.Calendar
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.TempoCalendar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
      [ (fromGregorian 2000 1 1, TempoDate (CommonMonth Shimotsuki) 25)
      , (fromGregorian 2000 1 2, TempoDate (CommonMonth Shimotsuki) 26)
      , (fromGregorian 2000 1 6, TempoDate (CommonMonth Shimotsuki) 30)
      , (fromGregorian 2000 1 7, TempoDate (CommonMonth Shiwasu) 1)
      , (fromGregorian 2001 5 22, TempoDate (CommonMonth Uzuki) 29)
      , (fromGregorian 2001 5 23, TempoDate (LeapMonth Uzuki) 1)
      , (fromGregorian 2010 1 1, TempoDate (CommonMonth Shimotsuki) 17)
      ] $ \(gregorian, tempo) ->
        it ("should convert the Gregorian date " ++ show gregorian ++ " into the Tempo date " ++ show tempo) $
          tempoDateFromGregorian jst gregorian `shouldBe` Just tempo
    forM_
      (filter (/= 2033) [2000..2100]) $ \(year) ->
        it ("should be defined in " ++ show year) $
          tempoDateFromGregorian jst (fromGregorian year 5 1) `shouldSatisfy` isJust
    it "should be indefinable in 2033" $
      tempoDateFromGregorian jst (fromGregorian 2033 5 1) `shouldBe` Nothing
