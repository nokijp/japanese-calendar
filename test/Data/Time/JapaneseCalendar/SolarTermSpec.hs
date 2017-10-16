module Data.Time.JapaneseCalendar.SolarTermSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.SolarTerm
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solarTermFromJapaneseName" $ do
    it "is a retraction of solarTermToJapaneseName" $ forAll arbitraryBoundedEnum $ \term ->
      solarTermFromJapaneseName (solarTermToJapaneseName term) == Just term
    it "should return Nothing when given an invalid name" $ solarTermFromJapaneseName "xxx" `shouldBe` Nothing

  describe "solarTerm" $ do
    forM_
      [ (fromGregorian 2000 3 20, VernalEquinox)
      , (fromGregorian 2000 4 4, ClearAndBright)
      , (fromGregorian 2001 3 5, AwakeningOfInsects)
      ] $ \(day, term) ->
        it ("should return the solar term " ++ show term ++ " when given the day " ++ show day) $
          solarTerm jst day `shouldBe` Just term
    forM_
      [ fromGregorian 2000 3 19
      , fromGregorian 2000 3 21
      , fromGregorian 2000 4 3
      , fromGregorian 2000 4 5
      , fromGregorian 2001 3 4
      , fromGregorian 2001 3 6
      ] $ \day ->
        it ("should return Nothing when given the day " ++ show day ++ " which is not a start day of a solar term") $
          solarTerm jst day `shouldBe` Nothing

  describe "nearestSolarTerm" $ do
    forM_
      [ (fromGregorian 2000 3 19, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 20, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 21, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 4 3, ClearAndBright, fromGregorian 2000 4 4)
      , (fromGregorian 2000 4 4, ClearAndBright, fromGregorian 2000 4 4)
      , (fromGregorian 2000 4 5, ClearAndBright, fromGregorian 2000 4 4)
      , (fromGregorian 2001 3 4, AwakeningOfInsects, fromGregorian 2001 3 5)
      , (fromGregorian 2001 3 5, AwakeningOfInsects, fromGregorian 2001 3 5)
      , (fromGregorian 2001 3 6, AwakeningOfInsects, fromGregorian 2001 3 5)
      , (fromGregorian 2010 3 20, VernalEquinox, fromGregorian 2010 3 21)
      , (fromGregorian 2010 3 21, VernalEquinox, fromGregorian 2010 3 21)
      , (fromGregorian 2010 3 22, VernalEquinox, fromGregorian 2010 3 21)
      ] $ \(day, term, termDay) ->
        it ("should return the nearest solar term " ++ show term ++ " and its date " ++ show termDay ++ " when given the day " ++ show day) $
          nearestSolarTerm jst day `shouldBe` (term, termDay)

  describe "findNearestSolarTerm" $ do
    forM_
      [ (fromGregorian 2000 3 19, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 20, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 21, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 4 4, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 20, ClearAndBright, fromGregorian 2000 4 4)
      ] $ \(day, term, termDay) ->
        it ("should return the date " ++ show termDay ++ " which is the start day of " ++ show term ++ " and the nearest from " ++ show day) $
          findNearestSolarTerm jst term day `shouldBe` termDay

jst :: TimeZone
jst = hoursToTimeZone 9
