module Data.Time.JapaneseCalendar.Internal.DataUtils
  ( enumToName
  , enumFromName
  ) where

import Data.List

enumToName :: Enum a => [String] -> a -> String
enumToName names enum = names !! fromEnum enum

enumFromName :: Enum a => [String] -> String -> Maybe a
enumFromName names name = toEnum <$> elemIndex name names
