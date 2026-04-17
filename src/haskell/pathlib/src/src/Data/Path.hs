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
import qualified Prelude

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

-- | An opaque path representation.
--
-- Comparison between paths isn't well defined, so we make this type explicitly
-- non-comparable. For example, we do not guarantee consistent treatment of
-- trailing / when they do not alter the path's meaning.
newtype Path = Path { unPath :: [Token] }

instance Show Path where
  show p = T.unpack ("Path(" <> toText p <> ")")

path :: Parser [Token]
path = many (component <|> slash)

slash :: Parser Token
slash = (many1 . char $ '/') *> pure Slash

component :: Parser Token
component = Component <$> text1 (noneOf "/")

-- | Build a t'Path' from a text representation.
--
-- This is built from a parser, so it can theoretically fail, hence the
-- t'HasCallStack' constraint.
fromText :: HasCallStack => Text -> Path
fromText = Path . fromJust . unsafeParseAll path

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
    tt Slash         = Nothing
    tt (Component a) = Just a
  in
    catMaybes . fmap tt . unPath

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
    Path . prefix absolute . intersperse Slash <$> traverse validateComponent cs

-- | Like 'fromComponentsMaybe', but throwing an exception if the result is
-- 'Nothing'.
fromComponentsThrow :: HasCallStack => Bool -> [Text] -> Path
fromComponentsThrow absolute =
  fromMaybe (error "One or more of the components has '/' in it")
  . fromComponentsMaybe absolute

validateComponent :: Text -> Maybe Token
validateComponent c =
  if T.elem '/' c then Nothing else (Just . Component $ c)

-- | A 'Text' representation of the t'Path'.
--
-- There is no guarantee that @toText . fromText@ is @id@.
--
-- >>> toText . fromText $ "//foo///bar///"
-- "/foo/bar/
toText :: Path -> Text
toText =
  let
    tt Slash         = "/"
    tt (Component a) = a
  in
    T.concat . fmap tt . unPath
