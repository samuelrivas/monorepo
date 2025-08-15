{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Reduplikation where

import           Perlude

import           Control.Monad        (replicateM_)
import           Data.Text            (intercalate)
import           Text.Parsec          (newline, noneOf)
import           Text.Parsec.Parselib (Parser, digitsAsNum, text1, unsafeParse)

example :: Text
example =
  intercalate
    "\n"
    [
      "hej",
      "3"
    ]

parser :: Parser (Text, Int)
parser =
  (,)
    <$> text1 (noneOf ['\n'])
    <* newline
    <*> digitsAsNum

main :: IO ()
main = do
  (text, reps) <- getContents >>= unsafeParse parser
  replicateM_ reps (putStr text)
