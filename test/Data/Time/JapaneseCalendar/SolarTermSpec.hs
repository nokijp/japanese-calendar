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
      , (fromGregorian 2010 3 21, VernalEquinox)
      , (fromGregorian 2010 4 5, ClearAndBright)
      , (fromGregorian 2011 3 6, AwakeningOfInsects)
      ] $ \(time, term) ->
        it ("should return the solar term " ++ show term ++ " when given the day " ++ show time) $
          solarTerm jst time `shouldBe` Just term
    forM_
      [ fromGregorian 2000 3 19
      , fromGregorian 2000 3 21
      , fromGregorian 2000 4 3
      , fromGregorian 2000 4 5
      , fromGregorian 2001 3 4
      , fromGregorian 2001 3 6
      ] $ \time ->
        it ("should return Nothing when given the day " ++ show time ++ " which is not a start day of a solar term") $
          solarTerm jst time `shouldBe` Nothing

jst :: TimeZone
jst = hoursToTimeZone 9
