{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Path (
  Path,
  fromText,
  isAbsolute,
  isRelative,
  fromComponents,
  components,
  toText
  ) where

import           Perlude

import           Data.List            (intersperse)
import           Data.Maybe           (catMaybes, fromJust, fromMaybe,
                                       listToMaybe)
import qualified Data.Text            as T
import           GHC.Base             ((<|>))
import           GHC.Stack            (HasCallStack)
import           Text.Parsec          (char, many, many1, noneOf)
import           Text.Parsec.Parselib (Parser, text1, unsafeParseAll)

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
    tt Slash         = Nothing
    tt (Component a) = Just a
  in
    catMaybes . fmap tt . unPath

fromComponents :: Bool -> [Text] -> Path
fromComponents True  = Path . (Slash :) . intersperse Slash . fmap Component
fromComponents False =  Path  . intersperse Slash . fmap Component

toText :: Path -> Text
toText =
  let
    tt Slash         = "/"
    tt (Component a) = a
  in
    T.concat . fmap tt . unPath
