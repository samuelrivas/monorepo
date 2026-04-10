{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Perlude

import           Data.Bool      (bool)
import           Data.List      (intersperse)
import qualified Data.Path      as Path
import           Data.Text      (intercalate)
import qualified Data.Text      as Text
import           Hedgehog       (MonadGen, Property, check, forAll, property,
                                 (/==), (===))
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
  property $ do
  (proto, text)  <- forAll $ testCaseGen
  let
    path = Path.fromText text
    cs = Path.components path
  cs === components proto
  leading proto === Path.isAbsolute path
  leading proto /== Path.isRelative path

anyCharGen :: MonadGen m => m Char
anyCharGen = Gen.frequency [(5, Gen.alphaNum), (2, Gen.latin1), (1, Gen.unicode)]

notSlashGen :: MonadGen m  => m Char
notSlashGen = Gen.filterT (/= '/') anyCharGen

componentGen :: MonadGen m => m Text
componentGen = Gen.text (Range.linear 1 100) notSlashGen

-- The trailing / becomes leading when the path is empty, so we need some logic
-- to set trailing to false in those cases
protoPathGen :: MonadGen m => m ProtoPath
protoPathGen =
  do
    cs <- Gen.list (Range.linear 0 100) componentGen
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
  pure ()
