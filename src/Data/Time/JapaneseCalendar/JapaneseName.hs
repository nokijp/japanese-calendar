{-# LANGUAGE TemplateHaskell #-}

module Data.Time.JapaneseCalendar.JapaneseName
  ( JapaneseName(..)
  , derivingJapaneseNameBoundedEnum
  ) where

import Control.Monad
import Language.Haskell.TH

-- | mutual conversion between values and Japanese names.
class JapaneseName a where
  -- | converts values to names.
  toJapaneseName :: a -> String
  -- | converts names to values if possible.
  fromJapaneseName :: String -> Maybe a

-- | generates an instance of JapaneseName with a name of type and Japanese names.
derivingJapaneseNameBoundedEnum :: Name -> [String] -> DecsQ
derivingJapaneseNameBoundedEnum typeName japaneseNames = do
  info <- reify typeName
  constructors <- case info of
    TyConI (DataD [] _ [] Nothing cs _) -> return cs
    _ -> fail "the type should take no parameters"
  constructorNames <- mapM
    ( \c -> case c of
        NormalC n [] -> return n
        _ -> fail "the all value constructors should take no parameters"
    ) constructors
  when (length constructorNames /= length japaneseNames) $ fail "the number of the names is too much or too few"

  -- toJapaneseName c = j
  let toClauses = zipWith (\c j -> Clause [ConP c []] (NormalB $ LitE $ StringL j) []) constructorNames japaneseNames
  -- fromJapaneseName j = Just c
  let fromClauses = zipWith (\c j -> Clause [LitP $ StringL j] (NormalB $ AppE (ConE 'Just) (ConE c)) []) constructorNames japaneseNames
  -- fromJapaneseName _ = Nothing
  let defaultFromClause = Clause [WildP] (NormalB $ ConE 'Nothing) []

  let toDec = FunD 'toJapaneseName toClauses
  let fromDec = FunD 'fromJapaneseName (fromClauses ++ [defaultFromClause])

  -- instance JapaneseName typeName where ...
  return [InstanceD Nothing [] (AppT (ConT ''JapaneseName) (ConT typeName)) [toDec, fromDec]]
