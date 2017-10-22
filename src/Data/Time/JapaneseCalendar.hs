module Data.Time.JapaneseCalendar
  ( jst
  ) where

import Data.Time.LocalTime

jst :: TimeZone
jst = hoursToTimeZone 9
