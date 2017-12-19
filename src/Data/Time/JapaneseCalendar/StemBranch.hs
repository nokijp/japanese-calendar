module Data.Time.JapaneseCalendar.StemBranch
  ( StemBranch(..)
  , Stem(..)
  , Branch(..)
  , stemBranchToJapaneseName
  , stemBranchFromJapaneseName
  , stemBranchToStemAndBranch
  , stemBranchFromStemAndBranch
  , stemToJapaneseName
  , stemFromJapaneseName
  , branchToJapaneseName
  , branchFromJapaneseName
  , yearStemBranch
  , monthStemBranch
  , dayStemBranch
  ) where

import Control.Monad
import Data.Time.Calendar
import Data.Time.JapaneseCalendar.Internal.DataUtils

-- | the stems and branches, 干支
data StemBranch =
    KinoeRat  -- ^ 甲子
  | KinotoOx  -- ^ 乙丑
  | HinoeTiger  -- ^ 丙寅
  | HinotoRabbit  -- ^ 丁卯
  | TsuchinoeDragon  -- ^ 戊辰
  | TsuchinotoSnake  -- ^ 己巳
  | KanoeHorse  -- ^ 庚午
  | KanotoGoat  -- ^ 辛未
  | MizunoeMonkey  -- ^ 壬申
  | MizunotoRooster  -- ^ 癸酉
  | KinoeDog  -- ^ 甲戌
  | KinotoPig  -- ^ 乙亥
  | HinoeRat  -- ^ 丙子
  | HinotoOx  -- ^ 丁丑
  | TsuchinoeTiger  -- ^ 戊寅
  | TsuchinotoRabbit  -- ^ 己卯
  | KanoeDragon  -- ^ 庚辰
  | KanotoSnake  -- ^ 辛巳
  | MizunoeHorse  -- ^ 壬午
  | MizunotoGoat  -- ^ 癸未
  | KinoeMonkey  -- ^ 甲申
  | KinotoRooster  -- ^ 乙酉
  | HinoeDog  -- ^ 丙戌
  | HinotoPig  -- ^ 丁亥
  | TsuchinoeRat  -- ^ 戊子
  | TsuchinotoOx  -- ^ 己丑
  | KanoeTiger  -- ^ 庚寅
  | KanotoRabbit  -- ^ 辛卯
  | MizunoeDragon  -- ^ 壬辰
  | MizunotoSnake  -- ^ 癸巳
  | KinoeHorse  -- ^ 甲午
  | KinotoGoat  -- ^ 乙未
  | HinoeMonkey  -- ^ 丙申
  | HinotoRooster  -- ^ 丁酉
  | TsuchinoeDog  -- ^ 戊戌
  | TsuchinotoPig  -- ^ 己亥
  | KanoeRat  -- ^ 庚子
  | KanotoOx  -- ^ 辛丑
  | MizunoeTiger  -- ^ 壬寅
  | MizunotoRabbit  -- ^ 癸卯
  | KinoeDragon  -- ^ 甲辰
  | KinotoSnake  -- ^ 乙巳
  | HinoeHorse  -- ^ 丙午
  | HinotoGoat  -- ^ 丁未
  | TsuchinoeMonkey  -- ^ 戊申
  | TsuchinotoRooster  -- ^ 己酉
  | KanoeDog  -- ^ 庚戌
  | KanotoPig  -- ^ 辛亥
  | MizunoeRat  -- ^ 壬子
  | MizunotoOx  -- ^ 癸丑
  | KinoeTiger  -- ^ 甲寅
  | KinotoRabbit  -- ^ 乙卯
  | HinoeDragon  -- ^ 丙辰
  | HinotoSnake  -- ^ 丁巳
  | TsuchinoeHorse  -- ^ 戊午
  | TsuchinotoGoat  -- ^ 己未
  | KanoeMonkey  -- ^ 庚申
  | KanotoRooster  -- ^ 辛酉
  | MizunoeDog  -- ^ 壬戌
  | MizunotoPig  -- ^ 癸亥
    deriving (Show, Eq, Bounded, Enum)

-- | returns a Japanese name of a stem-branch
stemBranchToJapaneseName :: StemBranch -> String
stemBranchToJapaneseName = (\(s, b) -> stemToJapaneseName s ++ branchToJapaneseName b) . stemBranchToStemAndBranch

-- | converts a Japanese name of a stem-branch into a StemBranch
stemBranchFromJapaneseName :: String -> Maybe StemBranch
stemBranchFromJapaneseName [stemName, branchName] =
  join $ stemBranchFromStemAndBranch <$> stemFromJapaneseName [stemName] <*> branchFromJapaneseName [branchName]
stemBranchFromJapaneseName _ = Nothing

-- | extracts a stem and a branch from a stem-branch
stemBranchToStemAndBranch :: StemBranch -> (Stem, Branch)
stemBranchToStemAndBranch stemBranch = (toEnum $ fromEnum stemBranch `mod` 10, toEnum $ fromEnum stemBranch `mod` 12)

-- | constructs a stem-branch from a stem and a branch
stemBranchFromStemAndBranch :: Stem -> Branch -> Maybe StemBranch
stemBranchFromStemAndBranch stem branch = if even diff then Just (toEnum stemBranchIndex) else Nothing
  where
    stemIndex = fromEnum stem
    branchIndex = fromEnum branch
    stemBranchIndex = diff * 5 + stemIndex
    diff = (stemIndex - branchIndex) `mod` 12

-- | the ten heavenly stems, 十干
data Stem =
    Kinoe  -- ^ 甲
  | Kinoto  -- ^ 乙
  | Hinoe  -- ^ 丙
  | Hinoto  -- ^ 丁
  | Tsuchinoe  -- ^ 戊
  | Tsuchinoto  -- ^ 己
  | Kanoe  -- ^ 庚
  | Kanoto  -- ^ 辛
  | Mizunoe  -- ^ 壬
  | Mizunoto  -- ^ 癸
    deriving (Show, Eq, Bounded, Enum)

japaneseStemNames :: [String]
japaneseStemNames = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]

-- | returns a Japanese name of a stem
stemToJapaneseName :: Stem -> String
stemToJapaneseName = enumToName japaneseStemNames

-- | converts a Japanese name of a stem into a Stem
stemFromJapaneseName :: String -> Maybe Stem
stemFromJapaneseName = enumFromName japaneseStemNames

-- | the twelve earthly branches, 十二支
data Branch =
    Rat  -- ^ 子
  | Ox  -- ^ 丑
  | Tiger  -- ^ 寅
  | Rabbit  -- ^ 卯
  | Dragon  -- ^ 辰
  | Snake  -- ^ 巳
  | Horse  -- ^ 午
  | Goat  -- ^ 未
  | Monkey  -- ^ 申
  | Rooster  -- ^ 酉
  | Dog  -- ^ 戌
  | Pig  -- ^ 亥
    deriving (Show, Eq, Bounded, Enum)

japaneseBranchNames :: [String]
japaneseBranchNames = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"]

-- | returns a Japanese name of a branch
branchToJapaneseName :: Branch -> String
branchToJapaneseName = enumToName japaneseBranchNames

-- | converts a Japanese name of a branch into a Branch
branchFromJapaneseName :: String -> Maybe Branch
branchFromJapaneseName = enumFromName japaneseBranchNames

-- | returns a stem-branch of a specified year
yearStemBranch :: Integer -> StemBranch
yearStemBranch year = toEnum $ fromIntegral $ (year + 56) `mod` 60

-- | returns a stem-branch of a specified month
monthStemBranch :: Integer -> Int -> StemBranch
monthStemBranch year month = toEnum $ fromIntegral $ (year * 12 + toInteger month + 13) `mod` 60

-- | returns a stem-branch of a specified day
dayStemBranch :: Day -> StemBranch
dayStemBranch day = toEnum $ fromIntegral $ (toModifiedJulianDay day + 50) `mod` 60
