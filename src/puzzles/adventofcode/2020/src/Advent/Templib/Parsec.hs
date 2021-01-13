{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- TODO: Move this to a lib, these are here so that we don't need to keep
-- recompiling adventlib while experimenting

-- TODO: Generalise to ParsecT where it makes sense

module Advent.Templib.Parsec (
  digitAsNum,
  digitsAsNum,
  textUntil,
  text,
  parse,
  unsafeParse
  ) where

import           Advent.Perlude
import qualified Prelude

import           Control.Monad.Fail (MonadFail)
import           Data.Char          (digitToInt)
import           Data.Foldable      (foldl')
import           Data.Text          (Text, pack)
import           Text.Parsec        (ParseError, anyChar, digit, lookAhead,
                                     many, many1, manyTill)
import qualified Text.Parsec        as Parsec
import           Text.Parsec.Text   (Parser)

-- Parse a single digit and return it as a Num value
digitAsNum :: Num n => Parser n
digitAsNum = fromIntegral . digitToInt <$> digit

-- Parse consecutive digits and return them as a Num value
digitsAsNum :: Num n => Parser n
digitsAsNum = foldl' (\acc n -> acc * 10 + n) 0 <$> many1 digitAsNum

-- Return the characters until p succeeds, as text. Doesn't consume p

-- TODO: I think this parser is probably not a good idea. Try to remove it
-- before promoting to common lib. What I don't like is that it leaves the
-- separator as not consumes, which goes against the principle of consuming all
-- you parse rather than expecting "external" separators. This makes this parser
-- more difficult to compose with other parsers following that principle
textUntil :: Parser a -> Parser Text
textUntil terminator = pack <$> manyTill anyChar (lookAhead terminator)

-- Consumes characters into a Text until the character parser fails
text :: Parser Char -> Parser Text
text = fmap pack . many

-- Run a parser over a Text. Returns 'Left' if the parser fails
parse :: Parser a -> Text -> Either ParseError a
parse = flip Parsec.parse "Advent.Templib.Parsec.parse"

-- Run a parser over a Text. Fails if the parser fails
unsafeParse :: MonadFail m => Parser a -> Text -> m a
unsafeParse p t =
  let result = parse p t
  in case result of
    Right a  -> pure a
    Left err -> fail . Prelude.show $ err
