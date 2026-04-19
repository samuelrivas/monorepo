{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

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
  mkComponentThrow,
  mkComponentMaybe,
  fromComponents,
  fromComponentsMaybe,
  fromComponentsThrow,
  components,
  toText
  ) where

import           Perlude
import qualified Prelude

import           Control.Monad.Trans.Class (lift)
import           Data.Coerce               (Coercible, coerce)
import           Data.List                 (intersperse)
import           Data.Maybe                (catMaybes, fromJust, fromMaybe,
                                            listToMaybe)
import qualified Data.Text                 as T
import           GHC.Base                  ((<|>))
import           GHC.Stack                 (HasCallStack)
import           Text.Parsec               (char, many, many1, noneOf)
import           Text.Parsec.Parselib      (Parser, text1, unsafeParseAll)

data Token = Slash | Name Text
  deriving stock (Show, Eq)

-- TODO Move FromText and ToText to library once we have more use cases
class ToText a where
  toText :: a -> Text

class FromTextEither a where
  fromTextEither :: Text -> Either Text a

class FromTextMaybe a where
  fromTextMaybe :: Text -> Maybe a

instance FromTextEither a => FromTextMaybe a where
  fromTextMaybe = either fail pure . fromTextEither

class FromTextThrow a where
  fromTextThrow :: Text -> a

instance FromTextEither a => FromTextThrow a where
  fromTextThrow = either error id . fromTextEither

class FromText a where
  fromText :: Text -> a

newtype Component = Component { unComponent :: Text }

instance Show Component where
  show c = T.unpack ("Component(" <> coerce c <> ")")

instance ToText Component where
  toText = coerce

-- instance FromTextEither Component where
--   fromTextEither =

-- | An opaque path representation.
--
-- Comparison between paths isn't well defined, so we make this type explicitly
-- non-comparable. For example, we do not guarantee consistent treatment of
-- trailing / when they do not alter the path's meaning.
newtype Path = Path { unPath :: [Token] }

instance Show Path where
  show p = T.unpack ("Path(" <> toText p <> ")")

instance ToText Path where
  toText = pathToText

instance FromText Path where
  fromText = parsePath

path :: Parser [Token]
path = many (name <|> slash)

slash :: Parser Token
slash = (many1 . char $ '/') *> pure Slash

name :: Parser Token
name = Name <$> text1 (noneOf "/")

-- | Build a t'Path' from a text representation.
--
-- This is built from a parser, so it can theoretically fail, hence the
-- t'HasCallStack' constraint.
parsePath :: HasCallStack => Text -> Path
parsePath = Path . fromJust . unsafeParseAll path

mkComponentMaybe :: Text -> Maybe Component
mkComponentMaybe t = if T.elem '/' t then Nothing else  Just . Component $ t

mkComponentThrow :: Text -> Component
mkComponentThrow t = fromMaybe (failInvalidComponent t) $ mkComponentMaybe t

failInvalidComponent :: Text -> a
failInvalidComponent t = error $ "'" <> t <> "' isn't a valid component name"


-- | Whether the t'Path' is absolute.
--
-- prop> isAbsolute a = not . isRelative $ a
--
-- >>> isAbsolute . fromText $ "foo/bar/baz"
-- False
-- >>> isAbsolute . fromText $ "/foo/bar/baz"
-- True
isAbsolute :: Path -> Bool
isAbsolute = fromMaybe False . fmap (== Slash) . listToMaybe . unPath

-- | Whether the t'Path' is relative.
isRelative :: Path -> Bool
isRelative = not . isAbsolute

-- | Get the components of a t'Path'
--
-- prop> components (fromComponents _ c) = c
--
-- >>> components . fromText $ "foo/bar/baz"
-- ["foo","bar","baz"]
components :: Path -> [Text]
components =
  let
    tt Slash    = Nothing
    tt (Name a) = Just a
  in
    catMaybes . fmap tt . unPath

fromComponents' :: Bool -> [Component] -> Path
fromComponents' absolute cs =
  let
    prefix True  = (Slash :)
    prefix False = id
  in
    Path . prefix absolute . intersperse Slash . fmap (Name . unComponent) $ cs

-- | Alias of 'fromComponentsMaybe'.
fromComponents :: Bool -> [Text] -> Maybe Path
fromComponents = fromComponentsMaybe

-- | Builds a t'Path' from a list of components.
fromComponentsMaybe ::
  Bool -- ^ Whether the path is absolute.
  -> [Text] -- ^ The list of components.
  -> Maybe Path -- ^ 'Nothing' if any of the components contains a @/@.
fromComponentsMaybe absolute cs =
  let
    prefix True  = (Slash :)
    prefix False = id
  in
    Path . prefix absolute . intersperse Slash <$> traverse validateNameMaybe cs

-- | Like 'fromComponentsMaybe', but throwing an exception if the result is
-- 'Nothing'.

-- TODO: FIX fhis duplication
fromComponentsThrow :: HasCallStack => Bool -> [Text] -> Path
fromComponentsThrow absolute =
  let
    prefix True  = (Slash :)
    prefix False = id
  in
    Path . prefix absolute . intersperse Slash . fmap validateNameThrow


-- TODO fix these two when Path gets a Component directly instead of a Text
validateNameMaybe :: Text -> Maybe Token
validateNameMaybe t = Name . unComponent <$>  mkComponentMaybe t

validateNameThrow :: HasCallStack => Text -> Token
validateNameThrow = Name . unComponent . mkComponentThrow

-- | A 'Text' representation of the t'Path'.
--
-- There is no guarantee that @toText . fromText@ is @id@.
--
-- >>> toText . fromText $ "//foo///bar///"
-- "/foo/bar/
pathToText :: Path -> Text
pathToText =
  let
    tt Slash    = "/"
    tt (Name a) = a
  in
    T.concat . fmap tt . unPath
