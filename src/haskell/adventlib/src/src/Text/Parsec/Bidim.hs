{-# LANGUAGE NoImplicitPrelude #-}

module Text.Parsec.Bidim (
  bidim
  ) where

import           Data.Bidim           (Bidim, fromText)
import           Perlude
import           Text.Parsec          (anyToken)
import           Text.Parsec.Parselib (Parser, text)

bidim :: (Char -> a) -> Parser (Bidim a)
bidim f = fmap f . fromText <$> text anyToken
