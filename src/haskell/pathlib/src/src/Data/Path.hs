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
  Path (isAbsolute),
  Component,
  FromText(..),
  fromText,
  isRelative,
  fromComponents,
  components,
  toText
  ) where

import           Perlude
import qualified Prelude

import           Data.Bifunctor       (first)
import           Data.Coerce          (coerce)
import           Data.Maybe           (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text            as T
import           GHC.Base             ((<|>))
import           GHC.Stack            (HasCallStack)
import           Text.Parsec          (char, many, many1, noneOf)
import           Text.Parsec.Parselib (Parser, parseAll, text1)

--
-- Parser
--

data Token = Slash | Name Text
  deriving stock (Show, Eq)

path :: Parser [Token]
path = many (name <|> slash)

slash :: Parser Token
slash = (many1 . char $ '/') *> pure Slash

name :: Parser Token
name = Name <$> text1 (noneOf "/")

--
-- FromText and ToText type class
--

-- TODO Move FromText and ToText to library once we have more use cases

-- | The inverse of 'FromText'.
--
-- prop> toText . fromTextThrow = id
class ToText a where
  toText :: a -> Text

-- | A class for types that can be parsed from 'Text'. Parsing may fail with a
-- 'Text' error message.
--
-- The minimal implementation requires 'fromTextEither'
class FromText a where
  fromTextEither :: Text -> Either Text a

  fromTextMaybe :: Text -> Maybe a
  fromTextMaybe = either fail pure . fromTextEither

  fromTextThrow :: HasCallStack => Text -> a
  fromTextThrow = either error id . fromTextEither

--
-- Component data type
--

-- | A path component
newtype Component = Component { unComponent :: Text }
  deriving stock Eq
  deriving newtype Ord

instance Show Component where
  show c = T.unpack ("Component(" <> coerce c <> ")")

instance ToText Component where
  toText = coerce

instance FromText Component where
  fromTextEither = mkComponentEither

mkComponentEither :: Text -> Either Text Component
mkComponentEither t =
  if T.elem '/' t
  then Left $ "'" <> t <> "' isn't a valid component name"
  else Right $ Component t

-- | An opaque path representation.
--
-- Comparison between paths isn't well defined, so we make this type explicitly
-- non-comparable. For example, we do not guarantee consistent treatment of
-- trailing / when they do not alter the path's meaning.
data Path = Path {
  isAbsolute :: Bool,
  components :: [Component]
  }

instance Show Path where
  show p = T.unpack ("Path(" <> toText p <> ")")

instance ToText Path where
  toText = pathToText

-- TODO Document that fromTextThrow cannot fail
instance FromText Path where
  fromTextEither = parsePathEither

toName :: Token -> Maybe Text
toName Slash    = Nothing
toName (Name n) = Just n

-- | Build a t'Path' from a text representation.
--
parsePathEither :: Text -> Either Text Path
parsePathEither txt =
  do
    tokens <- first show . parseAll path $ txt
    pure $ Path {
      isAbsolute = maybe False (Slash ==) $ Data.Maybe.listToMaybe tokens,
      components = Component <$> Data.Maybe.catMaybes (toName <$> tokens)
      }

--- This is an alias of 'fromTextThrow' as it is build off a parser. It is safe
--- to assume that it cannot fail, however.
fromText :: Text -> Path
fromText = fromTextThrow


-- | Whether the t'Path' is absolute.
--
-- prop> isAbsolute a = not . isRelative $ a
--
-- >>> isAbsolute . fromText $ "foo/bar/baz"
-- False
-- >>> isAbsolute . fromText $ "/foo/bar/baz"
-- True
isAbsolute' :: Path -> Bool
isAbsolute' = isAbsolute

-- | Whether the t'Path' is relative.
isRelative :: Path -> Bool
isRelative = not . isAbsolute

-- | Get the components of a t'Path'
--
-- prop> components (fromComponents _ c) = c
--
-- >>> components . fromText $ "foo/bar/baz"
-- ["foo","bar","baz"]
components' :: Path -> [Component]
components' = components

-- | Builds a t'Path' from a list of components.
fromComponents ::
  Bool -- ^ Whether the path is absolute.
  -> [Component] -- ^ The list of components.
  -> Path
fromComponents = Path

-- | A 'Text' representation of the t'Path'.
--
-- There is no guarantee that @toText . fromText@ is @id@.
--
-- >>> toText . fromText $ "//foo///bar///"
-- "/foo/bar"
pathToText :: Path -> Text
pathToText path =
  let
    heading = if isAbsolute path then "/" else ""
  in
    heading <> (T.intercalate "/" . fmap toText . components $ path)
