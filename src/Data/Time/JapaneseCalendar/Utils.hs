module Data.Time.JapaneseCalendar.Utils
  ( jst
  ) where

import Data.Time.LocalTime

jst :: TimeZone
jst = hoursToTimeZone 9
