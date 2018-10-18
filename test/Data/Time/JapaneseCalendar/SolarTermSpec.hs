module Data.Time.JapaneseCalendar.SolarTermSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.SolarTerm
import Data.Time.JapaneseCalendar.Utils
import Test.Hspec
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fromJapaneseName" $ do
    it "is a retraction of toJapaneseName" $ forAll arbitraryBoundedEnum $ \term ->
      fromJapaneseName (toJapaneseName (term :: SolarTerm)) == Just term
    it "should return Nothing when given an invalid name" $ (fromJapaneseName "xxx" :: Maybe SolarTerm) `shouldBe` Nothing

  describe "solarTerm" $ do
    forM_
      [ (fromGregorian 2000 3 20, VernalEquinox)
      , (fromGregorian 2000 4 4, ClearAndBright)
      , (fromGregorian 2001 3 5, AwakeningOfInsects)
      ] $ \(day, term) ->
        it ("should return a solar term " ++ show term ++ " when given a day " ++ show day) $
          solarTerm jst day `shouldBe` Just term
    forM_
      [ fromGregorian 2000 3 19
      , fromGregorian 2000 3 21
      , fromGregorian 2000 4 3
      , fromGregorian 2000 4 5
      , fromGregorian 2001 3 4
      , fromGregorian 2001 3 6
      ] $ \day ->
        it ("should return Nothing when given a day " ++ show day ++ " which is not a start day of a solar term") $
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
        it ("should return the nearest solar term " ++ show term ++ " and its date " ++ show termDay ++ " when given a day " ++ show day) $
          nearestSolarTerm jst day `shouldBe` (term, termDay)

  describe "solarTermsFrom" $ do
    it "should return a list of solar terms starting with the nearest solar term from a specified day" $
      take 3 (solarTermsFrom jst $ fromGregorian 2000 3 25) `shouldBe`
        [ (VernalEquinox, fromGregorian 2000 3 20)
        , (ClearAndBright, fromGregorian 2000 4 4)
        , (GrainRain, fromGregorian 2000 4 20)
        ]

  describe "findNearestSolarTerm" $ do
    forM_
      [ (fromGregorian 2000 3 19, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 20, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 21, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 4 4, VernalEquinox, fromGregorian 2000 3 20)
      , (fromGregorian 2000 3 20, ClearAndBright, fromGregorian 2000 4 4)
      ] $ \(day, term, termDay) ->
        it ("should return a date " ++ show termDay ++ " which is a start day of " ++ show term ++ " and the nearest from " ++ show day) $
          findNearestSolarTerm jst term day `shouldBe` termDay
