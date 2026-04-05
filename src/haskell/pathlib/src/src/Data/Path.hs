{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingVia #-}

module Data.Path where

import           Perlude

import           GHC.Base             ((<|>))
import           Text.Parsec          (char, many, many1, noneOf)
import           Text.Parsec.Parselib (Parser, text1, unsafeParseAll)
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import GHC.Stack (HasCallStack)

data Token = Slash | Component Text
  deriving stock (Show, Eq)

newtype Path = Path { unPath :: [Token] }
  deriving Show via [Token]

path :: Parser [Token]
path = many (component <|> slash)

slash :: Parser Token
slash = (many1 . char $ '/') *> pure Slash

component :: Parser Token
component = Component <$> text1 (noneOf "/")

fromText :: HasCallStack => Text -> Path
fromText = Path . fromJust . unsafeParseAll path

isAbsolute :: Path -> Bool
isAbsolute = fromMaybe False . fmap (== Slash) . listToMaybe . unPath

isRelative :: Path -> Bool
isRelative = not . isAbsolute
