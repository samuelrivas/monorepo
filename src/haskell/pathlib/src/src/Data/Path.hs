{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingVia #-}

module Data.Path where

import           Perlude

import Data.List (intersperse)
import           GHC.Base             ((<|>))
import           Text.Parsec          (char, many, many1, noneOf)
import           Text.Parsec.Parselib (Parser, text1, unsafeParseAll)
import Data.Maybe (fromJust, listToMaybe, fromMaybe, catMaybes)
import GHC.Stack (HasCallStack)
import qualified Data.Text as T

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

components :: Path -> [Text]
components =
  let
    toText Slash = Nothing
    toText (Component a) = Just a
  in
    catMaybes . fmap toText . unPath

fromComponents :: Bool -> [Text] -> Path
fromComponents True = Path . (Slash :) . intersperse Slash . fmap Component
fromComponents False =  Path  . intersperse Slash . fmap Component

toText :: Path -> Text
toText =
  let
    tt Slash = "/"
    tt (Component a) = a
  in
    T.concat . fmap tt . unPath
