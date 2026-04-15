{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | The t'Path' data type.
--
-- Minimal impiementation of the [POSIX
-- standard](https://pubs.opengroup.org/onlinepubs/9799919799/) for path names.
--
-- POSIX defines a special case for path names starting with @\/\/@. We
-- currently ignore this case, so @\/\/@ is equivalent to @\/@ in this library.

module Data.Path (
  Path,
  fromText,
  isAbsolute,
  isRelative,
  fromComponents,
  fromComponentsMaybe,
  fromComponentsThrow,
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

-- | Builds a t'Path' from a list of component names.
fromComponents ::
  Bool -- ^ Whether the path is absolute.
  -> [Text] -- ^ The list of components.
  -> Maybe Path -- ^ 'Nothing' if any of the components contains a @/@.
fromComponents = fromComponentsMaybe

fromComponentsMaybe ::
  Bool -- ^ Whether the path is absolute.
  -> [Text] -- ^ The list of components.
  -> Maybe Path -- ^ 'Nothing' if any of the components contains a @/@.
fromComponentsMaybe absolute cs =
  let
    prefix True  = (Slash :)
    prefix False = id
  in
    Path . prefix absolute . intersperse Slash <$> traverse validateComponent cs

fromComponentsThrow :: HasCallStack => Bool -> [Text] -> Path
fromComponentsThrow absolute =
  fromMaybe (error "One or more of the components has '/' in it")
  . fromComponentsMaybe absolute

validateComponent :: Text -> Maybe Token
validateComponent c =
  if T.elem '/' c then Nothing else (Just . Component $ c)

toText :: Path -> Text
toText =
  let
    tt Slash         = "/"
    tt (Component a) = a
  in
    T.concat . fmap tt . unPath
