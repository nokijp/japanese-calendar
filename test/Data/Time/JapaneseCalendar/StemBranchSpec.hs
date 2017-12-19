module Data.Time.JapaneseCalendar.StemBranchSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.StemBranch
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stemBranchToStemAndBranch" $ do
    forM_
      [ (KinoeRat, Kinoe, Rat)
      , (MizunotoRooster, Mizunoto, Rooster)
      , (KinoeDog, Kinoe, Dog)
      , (KinotoPig, Kinoto, Pig)
      , (HinoeRat, Hinoe, Rat)
      , (MizunotoPig, Mizunoto, Pig)
      ] $ \(stemBranch, stem, branch) ->
        it ("should extract " ++ show stem ++ " and " ++ show branch ++ " from " ++ show stemBranch) $
          stemBranchToStemAndBranch stemBranch `shouldBe` (stem, branch)

  describe "stemBranchFromStemAndBranch" $ do
    forM_
      [ (KinoeRat, Kinoe, Rat)
      , (MizunotoRooster, Mizunoto, Rooster)
      , (KinoeDog, Kinoe, Dog)
      , (KinotoPig, Kinoto, Pig)
      , (HinoeRat, Hinoe, Rat)
      , (MizunotoPig, Mizunoto, Pig)
      ] $ \(stemBranch, stem, branch) ->
        it ("should construct " ++ show stemBranch ++ " from " ++ show stem ++ " and " ++ show branch) $
          stemBranchFromStemAndBranch stem branch `shouldBe` Just stemBranch
    it "should return Nothing when given an impossible pair" $
      stemBranchFromStemAndBranch Kinoe Ox `shouldBe` Nothing

  describe "yearStemBranch" $ do
    forM_
      [ (1983, MizunotoPig)
      , (1984, KinoeRat)
      , (1985, KinotoOx)
      ] $ \(year, stemBranch) ->
        it ("should return " ++ show stemBranch ++ " when given " ++ show year) $
          yearStemBranch year `shouldBe` stemBranch

  describe "monthStemBranch" $ do
    forM_
      [ (2003, 10, MizunotoPig)
      , (2003, 11, KinoeRat)
      , (2003, 12, KinotoOx)
      , (2004, 11, HinoeRat)
      ] $ \(year, month, stemBranch) ->
        it ("should return " ++ show stemBranch ++ " when given a year" ++ show year ++ " and a month " ++ show month) $
          monthStemBranch year month `shouldBe` stemBranch

  describe "dayStemBranch" $ do
    forM_
      [ (fromGregorian 2000 1 6, MizunotoPig)
      , (fromGregorian 2000 1 7, KinoeRat)
      , (fromGregorian 2000 1 8, KinotoOx)
      ] $ \(day, stemBranch) ->
        it ("should return " ++ show stemBranch ++ " when given " ++ show day) $
          dayStemBranch day `shouldBe` stemBranch
