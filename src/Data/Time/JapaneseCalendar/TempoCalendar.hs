module Data.Time.JapaneseCalendar.TempoCalendar
  ( TempoMonthType(..)
  , TempoMonth(..)
  , TempoDate(..)
  , tempoMonthToJapaneseName
  , tempoMonthFromJapaneseName
  , nextTempoMonthType
  , previousTempoMonthType
  , tempoDateFromGregorian
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.JapaneseCalendar.Internal
import Data.Time.JapaneseCalendar.SolarTerm
import Data.Time.LocalTime

data TempoMonthType =
    Mutsuki  -- ^ 睦月
  | Kisaragi  -- ^ 如月
  | Yayoi  -- ^ 弥生
  | Uzuki  -- ^ 卯月
  | Satsuki  -- ^ 皐月
  | Minazuki  -- ^ 水無月
  | Fumizuki  -- ^ 文月
  | Hazuki  -- ^ 葉月
  | Nagatsuki  -- ^ 長月
  | Kannazuki  -- ^ 神無月
  | Shimotsuki  -- ^ 霜月
  | Shiwasu  -- ^ 師走
    deriving (Show, Eq, Bounded, Enum, Ord)

data TempoMonth =
    CommonMonth { tempoMonthType :: TempoMonthType }
  | LeapMonth { tempoMonthType :: TempoMonthType }
    deriving (Show, Eq)
instance Ord TempoMonth where
  (CommonMonth a) <= (CommonMonth b) = a <= b
  (CommonMonth a) <= (LeapMonth b) = a <= b
  (LeapMonth a) <= (CommonMonth b) = if a == b then False else a <= b
  (LeapMonth a) <= (LeapMonth b) = a <= b

data TempoDate = TempoDate { tempoYear :: Integer, tempoMonth :: TempoMonth, tempoDay :: Int } deriving (Show, Eq, Ord)

japaneseNames :: [String]
japaneseNames =
  [ "睦月"
  , "如月"
  , "弥生"
  , "卯月"
  , "皐月"
  , "水無月"
  , "文月"
  , "葉月"
  , "長月"
  , "神無月"
  , "霜月"
  , "師走"
  ]

-- | returns a Japanese name of a tempo month
tempoMonthToJapaneseName :: TempoMonth -> String
tempoMonthToJapaneseName (CommonMonth monthType) = japaneseNames !! fromEnum monthType
tempoMonthToJapaneseName (LeapMonth monthType) = "閏" ++ japaneseNames !! fromEnum monthType

-- | converts a Japanese name of a tempo month into a TempoMonth
tempoMonthFromJapaneseName :: String -> Maybe TempoMonth
tempoMonthFromJapaneseName name = toTempoMonth . toEnum <$> elemIndex ordinalName japaneseNames
  where
    isLeapMonth = "閏" `isPrefixOf` name
    ordinalName = if isLeapMonth then tail name else name
    toTempoMonth = if isLeapMonth then LeapMonth else CommonMonth

-- | the cyclic successor of TempoMonthType
nextTempoMonthType :: TempoMonthType -> TempoMonthType
nextTempoMonthType Shiwasu = Mutsuki
nextTempoMonthType monthType = succ monthType

-- | the cyclic predecessor of TempoMonthType
previousTempoMonthType :: TempoMonthType -> TempoMonthType
previousTempoMonthType Mutsuki = Shiwasu
previousTempoMonthType monthType = pred monthType

data TempoMonthInterval = TempoMonthInterval { _intervalTempoMonth :: TempoMonth, _intervalFirstDay :: Day, _intervalLastDay :: Day }
data MonthSeed = MonthSeed { _seedFirstDay :: Day, _seedLastDay :: Day, _seedCenterPoints :: [SolarTerm] }

-- | converts a Gregorian date into a Tempo date
tempoDateFromGregorian :: TimeZone -> Day -> Maybe TempoDate
tempoDateFromGregorian zone day = do
  intervals <- tempoMonthsInAYear zone day
  TempoMonthInterval month firstDay _ <- find (\(TempoMonthInterval _ firstDay lastDay) -> day >= firstDay && day <= lastDay) intervals
  let (TempoMonthInterval _ shimotsukiFirst _) = head intervals
  let (shimotsukiYear, _, _) = toGregorian shimotsukiFirst
  let year = if month >= CommonMonth Shimotsuki then shimotsukiYear else shimotsukiYear + 1
  return $ TempoDate year month (fromIntegral (diffDays day firstDay) + 1)

tempoMonthsInAYear :: TimeZone -> Day -> Maybe [TempoMonthInterval]
tempoMonthsInAYear zone day = singletonToMaybe (filter isLastKannazuki $ intervalCandidates seeds)
  where
    seeds = takeWhile (\(MonthSeed _ lastDay _) -> lastDay < nextWinterSolsticeDate) $ monthSeeds zone latestWinterSolsticeDate
    (latestWinterSolsticeDate, nextWinterSolsticeDate) = latestAndNextWinterSolsticeDate zone day
    isLastKannazuki = (\(TempoMonthInterval month _ _) -> tempoMonthType month == Kannazuki) . last
    singletonToMaybe [x] = Just x
    singletonToMaybe _ = Nothing

intervalCandidates :: [MonthSeed] -> [[TempoMonthInterval]]
intervalCandidates seeds = flip evalStateT Shimotsuki $ forM seeds $ \(MonthSeed firstDay lastDay centerPoints) ->
  if null centerPoints
  then StateT $ \monthType ->
    [ (TempoMonthInterval (CommonMonth monthType) firstDay lastDay, nextTempoMonthType monthType)
    , (TempoMonthInterval (LeapMonth (previousTempoMonthType monthType)) firstDay lastDay, monthType)
    ]
  else do
    monthType <- get
    put (nextTempoMonthType monthType)
    if isValidCommonMonth centerPoints monthType
    then return $ TempoMonthInterval (CommonMonth monthType) firstDay lastDay
    else lift []

monthSeeds :: TimeZone -> Day -> [MonthSeed]
monthSeeds zone latestWinterSolsticeDate = collectMonthSeeds intervals allCenterPoints
  where
    firsts = newMoonDatesFrom zone latestWinterSolsticeDate
    intervals = zipWith (\first next -> (first, addDays (-1) next)) firsts (tail firsts)
    allCenterPoints = centerPointsFrom zone latestWinterSolsticeDate

collectMonthSeeds :: [(Day, Day)] -> [(SolarTerm, Day)] -> [MonthSeed]
collectMonthSeeds ((firstDay, lastDay) : restIntervals) centerPoints = item : rest
  where
    item = MonthSeed firstDay lastDay (fst <$> centerPointsInMonth)
    rest = collectMonthSeeds restIntervals restCenterPoints
    (centerPointsInMonth, restCenterPoints) = span ((<= lastDay) . snd) centerPoints
collectMonthSeeds [] _ = []

latestAndNextWinterSolsticeDate :: TimeZone -> Day -> (Day, Day)
latestAndNextWinterSolsticeDate zone day = latestAndNext
  where
    nearest = findNearestSolarTerm zone WinterSolstice day
    shiftNearest days = findNearestSolarTerm zone WinterSolstice $ addDays days nearest
    latestAndNext =
      if nearest <= day
      then (nearest, shiftNearest 365)
      else (shiftNearest (-365), nearest)

newMoonDatesFrom :: TimeZone -> Day -> [Day]
newMoonDatesFrom zone day = utcTimeToDay <$> unfoldr (\d -> Just (d, succNewMoon d)) latest
  where
    utcStartTime = localTimeToUTC zone (LocalTime day midnight)
    addUTCDays days = addUTCTime $ fromRational $ days * 24 * 60 * 60
    nearest = nearestNewMoon utcStartTime
    latest =
      if nearest <= addUTCDays 1 utcStartTime
      then nearest
      else nearestNewMoon $ addUTCDays (-averageNewMoonCycle) nearest
    succNewMoon = nearestNewMoon . addUTCDays averageNewMoonCycle
    utcTimeToDay utcTime = localDay $ utcToLocalTime zone utcTime

centerPointsFrom :: TimeZone -> Day -> [(SolarTerm, Day)]
centerPointsFrom zone day = filter (isCenterPoint . fst) $ solarTermsFrom zone day

isValidCommonMonth :: [SolarTerm] -> TempoMonthType -> Bool
isValidCommonMonth centerPoints monthType = maybe True (== monthType) $ listToMaybe $ mapMaybe centerPointToMonthType centerPoints
  where
    centerPointToMonthType WinterSolstice = Just Shimotsuki
    centerPointToMonthType VernalEquinox = Just Kisaragi
    centerPointToMonthType SummerSolstice = Just Satsuki
    centerPointToMonthType AutumnalEquinox = Just Hazuki
    centerPointToMonthType _ = Nothing
