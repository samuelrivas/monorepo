{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Perlude

import           Data.Bool      (bool)
import           Data.Either    (isLeft)
import           Data.List      (intersperse)
import           Data.Maybe     (isNothing)
import qualified Data.Path      as Path
import qualified Data.Text      as Text
import           Hedgehog       (MonadGen, MonadTest, Property, annotateShow,
                                 assert, check, eval, evalEither, evalMaybe,
                                 failure, forAll, property, success, tripping,
                                 (/==), (===))
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

data ProtoPath = ProtoPath {
  components :: [Path.Component],
  leading    :: Bool,
  trailing   :: Bool
  }
  deriving stock Show

-- Generators
-- ----------

anyCharGen :: MonadGen m => m Char
anyCharGen = Gen.frequency [(5, Gen.alphaNum), (2, Gen.latin1), (1, Gen.unicode)]

notSlashGen :: MonadGen m  => m Char
notSlashGen = Gen.filterT (/= '/') anyCharGen

-- fromTextThrow fails if the component name contains a slash
componentGen :: MonadGen m => m Path.Component
componentGen = Path.fromTextThrow <$> Gen.text (Range.linear 1 100) notSlashGen

componentsGen :: MonadGen m => m [Path.Component]
componentsGen = Gen.list (Range.linear 0 100) componentGen

-- Generate a (invalid) component name with slashes in it
withSlashGen :: MonadGen m => m Text
withSlashGen =
  do
    a <- Gen.text (Range.linear 0 100) anyCharGen
    b <- Gen.text (Range.linear 0 100) anyCharGen
    s <- slashGen
    pure $ Text.concat [a, s, b]

-- Generates several component names where at least one of them contains a @/@.
invalidComponentNamesGen :: MonadGen m => m [Text]
invalidComponentNamesGen =
  do
    a <- fmap Path.toText <$> componentsGen
    b <- withSlashGen
    c <- fmap Path.toText <$> componentsGen
    pure $ a ++ [b] ++ c

-- The trailing / becomes leading when the path is empty, so we need some logic
-- to set trailing to false in those cases
protoPathGen :: MonadGen m => m ProtoPath
protoPathGen =
  do
    cs <- componentsGen
    l <- Gen.bool
    t <- ((not . null $ cs) &&) <$> Gen.bool
    pure $ ProtoPath cs l t

slashGen :: MonadGen m => m Text
slashGen = Gen.text (Range.linear 1 100) (Gen.constant '/')

textPathGen :: MonadGen m => ProtoPath -> m Text
textPathGen proto =
  let
    leadGen = genIf (leading proto) slashGen
    trailGen = genIf (trailing  proto) slashGen
    tGens = intersperse slashGen (pure <$> (Path.toText <$> components proto))
  in
    Text.concat <$> sequence (leadGen ++ tGens ++ trailGen)

testCaseGen :: MonadGen m => m (ProtoPath, Text)
testCaseGen =
  do
    proto <- protoPathGen
    text <- textPathGen proto
    pure (proto, text)

genIf :: MonadGen m => Bool -> m Text -> [m Text]
genIf b g = bool [] [g] b

-- Properties
-- ----------

propPathFromText :: Property
propPathFromText =
  property
  $ do
  (proto, text)  <- forAll testCaseGen
  let
    path = Path.fromText text
    cs = Path.components path
  cs === components proto
  leading proto === Path.isAbsolute path
  leading proto /== Path.isRelative path

propComponentRoundTrip :: Property
propComponentRoundTrip =
  property
  $ do
  c <- forAll componentGen
  tripping c Path.toText Path.fromTextMaybe

propComponentFail :: Property
propComponentFail =
  property
  $ do
  txt <- forAll withSlashGen
  let
    maybeComponent :: Maybe Path.Component = Path.fromTextMaybe txt
    eitherComponent :: Either Text Path.Component = Path.fromTextEither txt
  assert $ isNothing maybeComponent
  assert $ isLeft eitherComponent

evalFromTextSucceed ::
  MonadTest m =>
  Eq a =>
  Show a =>
  Path.FromText a =>
  Text -> m a
evalFromTextSucceed txt =
  do
    fromMaybe <- evalMaybe $ Path.fromTextMaybe txt
    fromEither <- evalEither $ Path.fromTextEither txt
    fromThrow <- eval $ Path.fromTextThrow txt
    fromMaybe === fromEither
    fromMaybe === fromThrow
    pure fromMaybe

propComponentSucceed :: Property
propComponentSucceed =
  property
  $ do
  txt <- forAll $ Path.toText <$> componentGen
  _ :: Path.Component <- evalFromTextSucceed txt
  success

propFromComponents :: Property
propFromComponents =
  property
  $ do
  (proto, _) <- forAll testCaseGen
  let
    path = Path.fromComponents (leading proto) (components proto)
    cs = Path.components path
  cs === components proto
  leading proto === Path.isAbsolute path
  leading proto /== Path.isRelative path

propRoundTrip :: Property
propRoundTrip =
  property
  $ do
  (_, t) <- forAll testCaseGen
  let
    path = Path.fromText t
    cs = Path.components path
    absolute = Path.isAbsolute path
    t' = Path.toText path
    path' = Path.fromText t'
    cs' = Path.components path'
    absolute' = Path.isAbsolute path'
  annotateShow t
  annotateShow t'
  cs === cs'
  absolute === absolute'

propFailOnSlash :: Property
propFailOnSlash =
  property
  $ do
  name <- forAll withSlashGen
  assert $ isNothing (Path.fromTextMaybe name :: Maybe Path.Component)

propMkComponentFail :: Property
propMkComponentFail =
  property
  $ do
  t <- forAll withSlashGen
  assert $ isNothing (Path.mkComponentMaybe t)

-- Test functions
-- --------------

main :: IO ()
main = do
  _ <- check propComponentRoundTrip
  _ <- check propComponentSucceed
  _ <- check propComponentFail
  _ <- check propPathFromText
  _ <- check propFromComponents
  _ <- check propRoundTrip
  _ <- check propFailOnSlash
  _ <- check propMkComponentFail
  pure ()
