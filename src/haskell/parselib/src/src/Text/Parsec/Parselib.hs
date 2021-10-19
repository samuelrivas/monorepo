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

module Text.Parsec.Parselib (
  digitAsNum,
  digitsAsNum,
  num,
  literal,
  parse,
  parseAll,
  parsePart,
  text,
  text1,
  textUntil,
  unsafeParse,
  unsafeParseAll,
  Parser
  ) where

import           Perlude
import qualified Prelude

import           Control.Monad.Fail (MonadFail)
import           Data.Char          (digitToInt)
import           Data.Foldable      (foldl')
import           Data.Functor       (($>))
import           Text.Parsec        (ParseError, anyChar, char, digit, eof,
                                     getInput, lookAhead, many, many1, manyTill,
                                     option, string, (<|>))
import qualified Text.Parsec        as Parsec
import           Text.Parsec.Text   (Parser)

-- Parse a single digit and return it as a Num value
digitAsNum :: Num n => Parser n
digitAsNum = fromIntegral . digitToInt <$> digit

-- Parse consecutive digits and return them as a Num value
digitsAsNum :: Num n => Parser n
digitsAsNum = foldl' (\acc n -> acc * 10 + n) 0 <$> many1 digitAsNum

-- Parse an integer number with optional sign
num :: Num n => Parser n
num =
  let
    sign = (char '+' $> (* 1)) <|> (char '-' $> (* (-1)))
  in do
    mult <- option (* 1) sign
    mult <$> digitsAsNum

-- Return the characters until p succeeds, as text. Doesn't consume p

-- TODO: I think this parser is probably not a good idea. Try to remove it
-- before promoting to common lib. What I don't like is that it leaves the
-- separator as not consumes, which goes against the principle of consuming all
-- you parse rather than expecting "external" separators. This makes this parser
-- more difficult to compose with other parsers following that principle

{-# DEPRECATED textUntil "Don't use this, use text with a parser that doesn't consume the terminator instead" #-}
textUntil :: Parser a -> Parser Text
textUntil terminator = pack <$> manyTill anyChar (lookAhead terminator)

-- Consumes characters into a Text until the character parser fails. Can return
-- an empty text.
text :: Parser Char -> Parser Text
text = fmap pack . many

-- Consumes at least character into a Text, continuing until the character
-- parser fails
text1 :: Parser Char -> Parser Text
text1 = fmap pack . many1

-- Consumes a literal, returning that literal
literal :: Text -> Parser Text
literal = fmap pack . string . unpack

-- Run a parser over a Text. Returns 'Left' if the parser fails
parse :: Parser a -> Text -> Either ParseError a
parse = flip Parsec.parse "Advent.Templib.Parsec.parse"

-- Run a parser over a Text, expecting it to consume all the input. Returns
-- 'Left' if the parser fails.
parseAll :: Parser a -> Text -> Either ParseError a
parseAll p = parse $ p <* eof

-- Run a parser over a Text, returning the parsed data and the text that the
-- parser didn't consume
parsePart :: Parser a -> Text -> Either ParseError (a, Text)
parsePart p = parse $ (,) <$> p <*> getInput

-- Run a parser over a Text. Fails if the parser fails
unsafeParse :: MonadFail m => Parser a -> Text -> m a
unsafeParse p t =
  let result = parse p t
  in case result of
    Right a  -> pure a
    Left err -> fail . Prelude.show $ err

-- Run a parser over a Text, expecting it to consume all the input. Fails if the
-- parser fails
unsafeParseAll :: MonadFail m => Parser a -> Text -> m a
unsafeParseAll p = unsafeParse $ p <* eof
