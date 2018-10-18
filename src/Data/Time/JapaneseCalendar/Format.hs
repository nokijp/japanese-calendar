{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Data.Time.JapaneseCalendar.Format
  ( formatTempoDate
  ) where

import Control.Arrow
import Data.Foldable
import Data.Time.JapaneseCalendar.JapaneseName
import Data.Time.JapaneseCalendar.TempoCalendar
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.String

-- | a format tool for 'TempoDate' using Template Haskell
--
-- @[formatTempoDate|%y年%M月%d日|]@ generates a function converting a 'TempoDate' into a 'String' like @"2000年1月2日"@
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
-- padding qualifiers like printf in C are also available
--
-- [@%2m@] pad with two spaces
--
-- [@%02m@] pad with two zeros
formatTempoDate :: QuasiQuoter
formatTempoDate = QuasiQuoter
  { quoteExp = quoteFormatExp
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

data Component =
    StringComponent String
  | FormatComponent ComponentType

data ComponentType =
    YearNum PadType  -- %y "2000"
  | LeapMonthName  -- %L "" or "閏"
  | MonthNum PadType  -- %m "1" - "12"
  | MonthFullNum PadType  -- %M "閏10"
  | MonthName  -- %b "神無月"
  | MonthFullName  -- %B "閏神無月"
  | DayNum PadType  -- %d "1" - "31"

data PadType =
    NonePad
  | SpacePad Int
  | ZeroPad Int

quoteFormatExp :: String -> ExpQ
quoteFormatExp str = do
  x <- newName "_x"
  components <- either fail return $ parseFormat str
  exps <- mapM (formatExp x) components
  lamE [varP x] $ foldlM (\a b -> [|$(return a) ++ $(return b)|]) (LitE $ StringL "") exps

formatExp :: Name -> Component -> ExpQ
formatExp _ (StringComponent s) = litE $ StringL s
formatExp x (FormatComponent (YearNum pt)) = [|$(padFunc pt) $ show $ tempoYear $(varE x)|]
formatExp x (FormatComponent LeapMonthName) = [|leapMonthSign $(varE x)|]
formatExp x (FormatComponent (MonthNum pt)) = [|$(padFunc pt) $ show $ monthNumber $ tempoMonth $(varE x)|]
formatExp x (FormatComponent (MonthFullNum pt)) = [|$(formatExp x $ FormatComponent LeapMonthName) ++ $(formatExp x $ FormatComponent $ MonthNum pt)|]
formatExp x (FormatComponent MonthName) = [|toJapaneseName $ tempoMonthType $ tempoMonth $(varE x)|]
formatExp x (FormatComponent MonthFullName) = [|$(formatExp x $ FormatComponent LeapMonthName) ++ $(formatExp x $ FormatComponent MonthName)|]
formatExp x (FormatComponent (DayNum pt)) = [|$(padFunc pt) $ show $ tempoDay $(varE x)|]

padFunc :: PadType -> ExpQ
padFunc NonePad = [|id|]
padFunc (SpacePad n) = [|pad ' ' $(litE $ IntegerL $ toInteger n)|]
padFunc (ZeroPad n) = [|pad '0' $(litE $ IntegerL $ toInteger n)|]

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
