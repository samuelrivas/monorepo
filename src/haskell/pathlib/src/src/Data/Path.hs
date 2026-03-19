{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Path where

import           Perlude

import           GHC.Base             ((<|>))
import           Text.Parsec          (char, many, many1, noneOf)
import           Text.Parsec.Parselib (Parser, text1)

data Token = Slash | Component Text
  deriving stock Show

path :: Parser [Token]
path = many (component <|> slash)

slash :: Parser Token
slash = (many1 . char $ '/') *> pure Slash

component :: Parser Token
component = Component <$> text1 (noneOf "/")
