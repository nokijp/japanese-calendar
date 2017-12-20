module Data.Time.JapaneseCalendar.RokuyoSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.Rokuyo
import Data.Time.JapaneseCalendar.TempoCalendar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toJapaneseName" $ do
    forM_
      [ (Sensho, "先勝")
      , (Tomobiki, "友引")
      , (Senbu, "先負")
      , (Butsumetsu, "仏滅")
      , (Taian, "大安")
      , (Shakko, "赤口")
      ] $ \(value, name) ->
        it ("should return " ++ show name ++ " when given " ++ show value) $
          toJapaneseName value `shouldBe` name

  describe "fromJapaneseName" $ do
    forM_
      [ ("先勝", Just Sensho)
      , ("友引", Just Tomobiki)
      , ("先負", Just Senbu)
      , ("仏滅", Just Butsumetsu)
      , ("大安", Just Taian)
      , ("赤口", Just Shakko)
      , ("", Nothing)
      , ("abc", Nothing)
      ] $ \(name, rokuyoMaybe) ->
        it ("should return " ++ show rokuyoMaybe ++ " when given " ++ show name) $
          fromJapaneseName name `shouldBe` rokuyoMaybe

  describe "rokuyoFromTempoDate" $ do
    forM_
      [ (TempoDate 2000 (CommonMonth Mutsuki) 1, Sensho)
      , (TempoDate 2000 (CommonMonth Mutsuki) 2, Tomobiki)
      , (TempoDate 2000 (CommonMonth Mutsuki) 3, Senbu)
      , (TempoDate 2000 (CommonMonth Mutsuki) 4, Butsumetsu)
      , (TempoDate 2000 (CommonMonth Mutsuki) 5, Taian)
      , (TempoDate 2000 (CommonMonth Mutsuki) 6, Shakko)
      , (TempoDate 2000 (CommonMonth Mutsuki) 7, Sensho)
      , (TempoDate 2000 (CommonMonth Kisaragi) 1, Tomobiki)
      , (TempoDate 2000 (CommonMonth Kisaragi) 2, Senbu)
      , (TempoDate 2000 (CommonMonth Kisaragi) 3, Butsumetsu)
      , (TempoDate 2000 (CommonMonth Kisaragi) 4, Taian)
      , (TempoDate 2000 (CommonMonth Kisaragi) 5, Shakko)
      , (TempoDate 2000 (CommonMonth Kisaragi) 6, Sensho)
      , (TempoDate 2000 (CommonMonth Yayoi) 1, Senbu)
      , (TempoDate 2000 (CommonMonth Uzuki) 1, Butsumetsu)
      , (TempoDate 2000 (CommonMonth Satsuki) 1, Taian)
      , (TempoDate 2000 (CommonMonth Minazuki) 1, Shakko)
      , (TempoDate 2000 (CommonMonth Fumizuki) 1, Sensho)
      , (TempoDate 2000 (CommonMonth Hazuki) 1, Tomobiki)
      , (TempoDate 2000 (CommonMonth Nagatsuki) 1, Senbu)
      , (TempoDate 2000 (CommonMonth Kannazuki) 1, Butsumetsu)
      , (TempoDate 2000 (CommonMonth Shimotsuki) 1, Taian)
      , (TempoDate 2000 (CommonMonth Shiwasu) 1, Shakko)
      ] $ \(date, expected) ->
        it ("should return a rokuyo " ++ show expected ++ " when given a date " ++ show date) $
          rokuyoFromTempoDate date `shouldBe` expected

  describe "rokuyo" $ do
    it "should run" $
      rokuyo jst (fromGregorian 2000 1 1) `shouldBe` Just Taian
