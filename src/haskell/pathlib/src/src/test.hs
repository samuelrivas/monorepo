{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Perlude

import           Data.Bool      (bool)
import           Data.List      (intersperse)
import           Data.Maybe     (isNothing)
import qualified Data.Path      as Path
import qualified Data.Text      as Text
import           Hedgehog       (MonadGen, Property, annotateShow, assert,
                                 check, evalMaybe, forAll, property, (/==),
                                 (===))
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

data ProtoPath = ProtoPath {
  components :: [Text],
  leading    :: Bool,
  trailing   :: Bool
  }
  deriving stock Show

propFromText :: Property
propFromText =
  property
  $ do
  (proto, text)  <- forAll testCaseGen
  let
    path = Path.fromText text
    cs = Path.components path
  cs === components proto
  leading proto === Path.isAbsolute path
  leading proto /== Path.isRelative path

propFromComponents :: Property
propFromComponents =
  property
  $ do
  (proto, _) <- forAll testCaseGen
  path <- evalMaybe $ Path.fromComponents (leading proto) (components proto)
  let
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
  cs <- forAll invalidComponentsGen
  a <- forAll Gen.bool
  assert $ isNothing (Path.fromComponents a cs)

propMkComponentFail :: Property
propMkComponentFail =
  property
  $ do
  t <- forAll withSlashGen
  assert $ isNothing (Path.mkComponentMaybe t)

anyCharGen :: MonadGen m => m Char
anyCharGen = Gen.frequency [(5, Gen.alphaNum), (2, Gen.latin1), (1, Gen.unicode)]

notSlashGen :: MonadGen m  => m Char
notSlashGen = Gen.filterT (/= '/') anyCharGen

componentGen :: MonadGen m => m Text
componentGen = Gen.text (Range.linear 1 100) notSlashGen

componentsGen :: MonadGen m => m [Text]
componentsGen = Gen.list (Range.linear 0 100) componentGen

-- Generate a (invalid) component name with slashes in it
withSlashGen :: MonadGen m => m Text
withSlashGen =
  do
    a <- Gen.text (Range.linear 0 100) anyCharGen
    b <- Gen.text (Range.linear 0 100) anyCharGen
    s <- slashGen
    pure $ Text.concat [a, s, b]

invalidComponentsGen :: MonadGen m => m [Text]
invalidComponentsGen =
  do
    a <- componentsGen
    b <- withSlashGen
    c <- componentsGen
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
    tGens = intersperse slashGen (pure <$> components proto)
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

main :: IO ()
main = do
  _ <- check propFromText
  _ <- check propFromComponents
  _ <- check propRoundTrip
  _ <- check propFailOnSlash
  _ <- check propMkComponentFail
  pure ()
