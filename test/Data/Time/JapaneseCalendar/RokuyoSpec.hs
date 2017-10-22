module Data.Time.JapaneseCalendar.RokuyoSpec
  ( main
  , spec
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar
import Data.Time.JapaneseCalendar.Rokuyo
import Data.Time.JapaneseCalendar.TempoCalendar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
      ] $ \(date, rokuyo) ->
        it ("should return the rokuyo " ++ show rokuyo ++ " when given the date " ++ show date) $
          rokuyoFromTempoDate date `shouldBe` rokuyo

  describe "rokuyoFromGregorian" $ do
    it "should run" $
      rokuyoFromGregorian jst (fromGregorian 2000 1 1) `shouldBe` Just Taian
