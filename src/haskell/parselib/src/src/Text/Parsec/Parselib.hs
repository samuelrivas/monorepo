{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Commonly used parsers and parsing functions.

-- TODO: Generalise to ParsecT where it makes sense

module Text.Parsec.Parselib (
  -- * Parsers
  digitAsNum,
  digitsAsNum,
  num,
  literal,
  linesOf,
  bit,
  bitString,
  matrix,
  text,
  text1,
  -- * Parsing functions
  parse,
  parseAll,
  parsePart,
  unsafeParse,
  unsafeParseAll,
  Parser
  ) where

import           Perlude

import           Data.Char        (digitToInt)
import           Data.Functor     (($>))
import qualified Text.Parsec      as Parsec
import           Text.Parsec      (ParseError, char, digit, eof, getInput, many,
                                   many1, option, sepEndBy, string, (<|>))
import           Text.Parsec.Text (Parser)

-- | Parse a single digit and return it as a 'Num' value.
digitAsNum :: Num n => Parser n
digitAsNum = fromIntegral . digitToInt <$> digit

-- | Parse consecutive digits and return them as a 'Num' value.
digitsAsNum :: Num n => Parser n
digitsAsNum = foldl' (\acc n -> acc * 10 + n) 0 <$> many1 digitAsNum

-- | Parse a 'Num' with optional sign.
num :: Num n => Parser n
num =
  let
    sign = (char '+' $> (* 1)) <|> (char '-' $> (* (-1)))
  in do
    mult <- option (* 1) sign
    mult <$> digitsAsNum

-- | Consumes characters into a 'Text' until the character parser fails. Can
-- return an empty 'Text'.
text :: Parser Char -> Parser Text
text = fmap pack . many

-- | Consumes at least character into a 'Text', continuing until the character
-- parser fails.
text1 :: Parser Char -> Parser Text
text1 = fmap pack . many1

-- | Consumes a literal, returning that literal.
literal :: Text -> Parser Text
literal = fmap pack . string . unpack

-- | Consumes an input line by line, parsing each line with the given parser.
linesOf :: Parser a -> Parser [a]
linesOf p = p `sepEndBy` char '\n'

-- | Parses 1 as 'True' and 0 as 'False'.
bit :: Parser Bool
bit = (literal "1" $> True) <|> (literal "0" $> False)

-- | Parses a input of 1s or 0s as a list of bits.
bitString :: Parser [Bool]
bitString = many1 bit

-- | Consumes input as a matrix, where columns are separated by one or many
-- spaces and rows by exactly one end of line.
matrix :: Parser a -> Parser [[a]]
matrix p =
  let cell = many (char ' ') *> p
  in linesOf $ many1 cell

-- | Run a parser over a 'Text'. Returns 'Left' if the parser fails.
parse :: Parser a -> Text -> Either ParseError a
parse = flip Parsec.parse "Text.Parsec.Parselib.parse"

-- | Run a parser over a 'Text', expecting it to consume all the input. Returns
-- 'Left' if the parser fails.
parseAll :: Parser a -> Text -> Either ParseError a
parseAll p = parse $ p <* eof

-- | Run a parser over a 'Text', returning the parsed data and the text that the
-- parser didn't consume.
parsePart :: Parser a -> Text -> Either ParseError (a, Text)
parsePart p = parse $ (,) <$> p <*> getInput

-- | Run a parser over a 'Text'. Fails if the parser fails
unsafeParse :: MonadFail m => Parser a -> Text -> m a
unsafeParse p t =
  let result = parse p t
  in case result of
    Right a  -> pure a
    Left err -> fail . show $ err

-- | Run a parser over a 'Text', expecting it to consume all the input. Fails if
-- the parser fails.
unsafeParseAll :: MonadFail m => Parser a -> Text -> m a
unsafeParseAll p = unsafeParse $ p <* eof
