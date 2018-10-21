{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveLift #-}

module Data.Time.JapaneseCalendar.Format
  ( formatTempoDate
  , qFormatTempoDate
  ) where

import Control.Arrow
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.TempoCalendar
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.String

-- | a formatter for 'TempoDate'.
--
-- @formatTempoDate "%y年%M月%d日"@ converts a 'TempoDate' into a 'String' like @"2000年1月2日"@.
--
-- [@%y@] year (e.g. @"2000"@)
--
-- [@%L@] leap month sign, @""@ or @"閏"@
--
-- [@%m@] month, @"1"@ - @"12"@
--
-- [@%M@] same as @%L%m@ (e.g. @"閏10"@)
--
-- [@%b@] month in Japanese (e.g. @"神無月"@)
--
-- [@%B@] same as @%L%b@ (e.g. @"閏神無月"@)
--
-- [@%d@] day, @"1"@ - @"31"@
--
-- padding qualifiers like printf in C are also available.
--
-- [@%2m@] pad with two spaces
--
-- [@%02m@] pad with two zeros
formatTempoDate :: String -> TempoDate -> Either String String
formatTempoDate str td = do
  components <- parseFormat str
  return $ format components td

-- | a Template Haskell version of 'formatTempoDate', which generates a formatter at compile-time.
--
-- if a given format is invalid, compilation will be failed.
--
-- @[qFormatTempoDate|fmt|]@ is semantically the same as @formatTempoDate "fmt"@.
qFormatTempoDate :: QuasiQuoter
qFormatTempoDate = QuasiQuoter
  { quoteExp = quoteFormatExp
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

quoteFormatExp :: String -> ExpQ
quoteFormatExp str = do
  components <- either fail return $ parseFormat str
  [|format components|]

data Component =
    StringComponent String
  | FormatComponent ComponentType
    deriving Lift

data ComponentType =
    YearNum PadType  -- %y "2000"
  | LeapMonthName  -- %L "" or "閏"
  | MonthNum PadType  -- %m "1" - "12"
  | MonthFullNum PadType  -- %M "閏10"
  | MonthName  -- %b "神無月"
  | MonthFullName  -- %B "閏神無月"
  | DayNum PadType  -- %d "1" - "31"
    deriving Lift

data PadType =
    NonePad
  | SpacePad Int
  | ZeroPad Int
    deriving Lift

format :: [Component] -> TempoDate -> String
format components td = foldMap (flip formatComponent td) components

formatComponent :: Component -> TempoDate -> String
formatComponent (StringComponent s) _ = s
formatComponent (FormatComponent (YearNum pt)) td = padFunc pt $ show $ tempoYear td
formatComponent (FormatComponent LeapMonthName) td = leapMonthSign td
formatComponent (FormatComponent (MonthNum pt)) td = padFunc pt $ show $ monthNumber $ tempoMonth td
formatComponent (FormatComponent (MonthFullNum pt)) td = formatComponent (FormatComponent LeapMonthName) td ++ formatComponent (FormatComponent $ MonthNum pt) td
formatComponent (FormatComponent MonthName) td = toJapaneseName $ tempoMonthType $ tempoMonth td
formatComponent (FormatComponent MonthFullName) td = formatComponent (FormatComponent LeapMonthName) td ++ formatComponent (FormatComponent MonthName) td
formatComponent (FormatComponent (DayNum pt)) td = padFunc pt $ show $ tempoDay td

padFunc :: PadType -> String -> String
padFunc NonePad = id
padFunc (SpacePad n) = pad ' ' n
padFunc (ZeroPad n) = pad '0' n

pad :: Char -> Int -> String -> String
pad c n s = let padLength = n - length s in if padLength > 0 then replicate padLength c ++ s else s

leapMonthSign :: TempoDate -> String
leapMonthSign td = if isLeapMonth $ tempoMonth td then "閏" else ""

parseFormat :: String -> Either String [Component]
parseFormat = left show . parse parser ""

parser :: Parser [Component]
parser = many (stringComponentParser <|> formatComponentParser) <* eof

stringComponentParser :: Parser Component
stringComponentParser = StringComponent <$> many1 (satisfy (/= '%'))

formatComponentParser :: Parser Component
formatComponentParser = char '%' *> ( percentParser
                                    <|> try yearNumParser
                                    <|> try leapMonthNameParser
                                    <|> try monthNumParser
                                    <|> try monthFullNumParser
                                    <|> try monthNameParser
                                    <|> try monthFullNameParser
                                    <|> try dayNumParser
                                    )

percentParser :: Parser Component
percentParser = StringComponent "%" <$ char '%'

yearNumParser :: Parser Component
yearNumParser = numParser 'y' YearNum

leapMonthNameParser :: Parser Component
leapMonthNameParser = nonePadParser 'L' LeapMonthName

monthNumParser :: Parser Component
monthNumParser = numParser 'm' MonthNum

monthFullNumParser :: Parser Component
monthFullNumParser = numParser 'M' MonthFullNum

monthNameParser :: Parser Component
monthNameParser = nonePadParser 'b' MonthName

monthFullNameParser :: Parser Component
monthFullNameParser = nonePadParser 'B' MonthFullName

dayNumParser :: Parser Component
dayNumParser = numParser 'd' DayNum

numParser :: Char -> (PadType -> ComponentType) -> Parser Component
numParser c t = FormatComponent . t <$> (modifierParser <* char c)

modifierParser :: Parser PadType
modifierParser = (ZeroPad <$> (char '0' *> intParser)) <|> (SpacePad <$> intParser) <|> return NonePad

intParser :: Parser Int
intParser = read <$> ((:) <$> satisfy (\c -> c >= '1' && c <= '9') <*> many digit)

nonePadParser :: Char -> ComponentType -> Parser Component
nonePadParser c t = FormatComponent t <$ char c
